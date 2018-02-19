unit untCOM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, synaser, strutils;

const
  MaxError = 10;

type

  ECOMState = (COMError, COMStopped, COMTrial, COMRunning);

  RSlave = record
    stringlist: TStringList;
    Cell_count: integer;
    Cell_U: array[0..11] of single;
    Cell_Bleed: array[0..11] of boolean;
    Cell_Open: array[0..11] of boolean;
  end;

  RGeneral = record
    stringlist: TStringList;
    Slave_count: cardinal;
    Cell_count: cardinal;
    Battery_I: single;
    Max_U, Min_U: single;
    SOC: integer;
    OV: single;
  end;

  { TFrame }

  TFrame = class
    protected
      FrameLines: TStringList;
      Slaves: array[0..15] of RSlave;
      General: RGeneral;
    public
      constructor Create;
      destructor Free;

      procedure putFrameLines(lines: string);
      procedure AnalyseFrame;
      procedure AnalyseSlave(var target: RSlave; line: string);
      procedure AnalyseGeneral(var target: RGeneral; line: string);

      function getIbat: real;
      function getSOC: real;
      function getUmin: real;
      function getUmax: real;
      function getUabs: real;
      function getDU: real;
      function getOV: real;
      function getCellCount: integer;
      function getSlaveCount: integer;
      function getCellU(index: integer): real;
  end;

  TFuncFrameReturn = procedure(frame: TFrame) of object;

  { TCOM }

  TCOM = class(TThread)
    protected
      ser: TBlockSerial;
      ports: TStringList;
      LastFrame: TFrame;

      COMState: ECOMState;
      Connected: boolean;
      RecvBuffer: string;
      ErrorCounter: integer;

      function check_device: boolean;
      function open_port: boolean;
      procedure updateAvailablePorts;
      procedure DecodeBuffer;

      procedure Execute; override;
    public
      frameReturn: TFuncFrameReturn;
      retryConnection: boolean;

      Constructor Create(CreateSuspended: boolean);
      Destructor Free;

      function isConneted: boolean;
      function isTrying: boolean;

      procedure CMDPing;
      procedure CMDStdChg;
      procedure CMDFullChg;
      procedure CMDStartChg;
  end;


var
  COM: TCOM;


implementation

{ TFrame }

constructor TFrame.Create;
var
  i, j: integer;
begin
  FrameLines:= TStringList.Create;
  FrameLines.StrictDelimiter:= true;
  FrameLines.Delimiter:= AnsiChar(10);

  for i:= 0 to 15 do
  begin
    Slaves[i].stringlist:= TStringList.Create;
    Slaves[i].Cell_count:= 0;
    for j:= 0 to 11 do
        Slaves[i].Cell_U[j]:= 0;
  end;

  General.stringlist:= TStringList.Create;
  General.Slave_count:= 0;
  General.Cell_count:= 0;
  General.Battery_I:= 0;
  General.Max_U:= 0;
  General.Min_U:= 0;
end;

destructor TFrame.Free;
var
  i: integer;
begin
  for i:= 0 to 15 do
    Slaves[i].stringlist.Free;
  General.stringlist.Free;
end;

procedure TFrame.putFrameLines(lines: string);
begin
  FrameLines.Clear;
  FrameLines.DelimitedText:= lines;
end;

procedure TFrame.AnalyseFrame;
var
  i, slave: integer;
begin
  i:= 0;
  while Pos('---BMS', FrameLines.Strings[i]) = 0 do
  begin
    AnalyseGeneral(General, FrameLines.Strings[i]);
    inc(i);
  end;

  for slave:= 0 to 15 do
    Slaves[slave].stringlist.Clear;

  slave:= -1;
  while Pos('---End Frame---', FrameLines.Strings[i]) = 0 do
  begin
    if Pos('---BMS', FrameLines.Strings[i]) > 0 then
      inc(slave);

    Slaves[slave].stringlist.Add(FrameLines.Strings[i]);
    AnalyseSlave(Slaves[slave], FrameLines.Strings[i]);
    inc(i);
  end;
end;

procedure TFrame.AnalyseSlave(var target: RSlave; line: string);
var
  lab, val: string;
  sep, i, j: cardinal;
begin
  sep:= Pos(':', line);
  lab:= LeftStr(line, sep-1);
  val:= RightStr(line, length(line)-sep-1);
  case lab of
    'Health':;
    'ITMP':;
    'Cell Test Passed':;
    'GPIO Test Passed':;
    'Status Test Passed':;
    'MUX Test Passed':;
    'Secondary Reference':;
    'Internal Temperatur':;
    'VA':;
    'VD':;
    'Balancing':;
    'Cell1',
    'Cell2',
    'Cell3',
    'Cell4',
    'Cell5',
    'Cell6',
    'Cell7',
    'Cell8',
    'Cell9',
    'Cell10',
    'Cell11',
    'Cell12':
    begin
      i:= StrToInt(RightStr(lab, length(lab)-4)) - 1;
      j:= Pos('mV', val);
      target.Cell_U[i]:= StrToInt(LeftStr(val, j-1))/1000;

      if Pos('Bleed', val) > 0 then
        target.Cell_Bleed[i]:= true
      else
        target.Cell_Bleed[i]:= false;

      if Pos('Open', val) > 0 then
        target.Cell_Open[i]:= true
      else
        target.Cell_Open[i]:= false;

      target.Cell_count:= i+1;
    end;
    'CC':;
  end;
end;

procedure TFrame.AnalyseGeneral(var target: RGeneral; line: string);
var
  lab, val: string;
  sep: cardinal;
begin
  sep:= Pos(':', line);
  lab:= LeftStr(line, sep-1);
  val:= RightStr(line, length(line)-sep-1);
  case lab of
    'CellCount':
      target.Cell_count:= StrToInt(val);
    'Slavecount':
      target.Slave_count:= StrToInt(val);
    'Max U':
      target.Max_U:= StrToInt(LeftStr(val, length(val)-2))/1000;
    'Min U':
      target.Min_U:= StrToInt(LeftStr(val, length(val)-2))/1000;
    'Battery I':
      target.Battery_I:= StrToInt(LeftStr(val, length(val)-2))/1000;
    'Balancing':;
    'SOC':
      target.SOC:= StrToInt(LeftStr(val, length(val)-1));
    'BMS_OV':
      target.OV:= StrToInt(LeftStr(val, length(val)-2))/1000;
  end;
end;

function TFrame.getIbat: real;
begin
  Result:= General.Battery_I;
end;

function TFrame.getSOC: real;
begin
  Result:= General.SOC;
end;

function TFrame.getUmin: real;
begin
  Result:= General.Min_U;
end;

function TFrame.getUmax: real;
begin
  Result:= General.Max_U;
end;

function TFrame.getUabs: real;
var
  avr: real;
begin
  avr:= (General.Max_U + General.Min_U)/2;
  Result:= General.Cell_count*avr;
end;

function TFrame.getDU: real;
begin
  Result:= General.Max_U - General.Min_U;
end;

function TFrame.getOV: real;
begin
  Result:= General.OV;
end;

function TFrame.getCellCount: integer;
begin
  Result:= General.Cell_count;
end;

function TFrame.getSlaveCount: integer;
begin
  Result:= General.Slave_count;
end;

function TFrame.getCellU(index: integer): real;
var
  i, slave: integer;
begin
  i:= index;
  slave:= 0;
  while i >= Slaves[slave].Cell_count do
  begin
    dec(i, Slaves[slave].Cell_count);
    inc(slave);
  end;
  Result:= Slaves[slave].Cell_U[i];
end;

function TCOM.check_device: boolean;
var
  line: string;
  i, p: integer;
begin
  Result:= false;
  ser.SendString('NeoBMS ping' + #10);

  i:= 0;
  repeat
    line:= ser.Recvstring(100);
    p:= Pos('---NeoBMS---', line);
    inc(i);
  until (p <> 0) or (i > 10) or (ser.LastError <> 0);

  if p = 0 then
    exit;

  Result:= true;
end;

procedure TCOM.Execute;
begin
  repeat
    case COMState of
      COMStopped:
      begin
        if retryConnection then
        begin
          retryConnection:= false;
          COMState:= COMTrial;
        end;
      end;

      COMTrial:
      begin
        if open_port then
        begin
          Connected:= true;
          COMState:= COMRunning;
        end
        else
        begin
          Connected:= false;
          COMState:= COMStopped;
        end;
      end;

      COMRunning:
      begin
        AppendStr(RecvBuffer, ser.RecvPacket(1));
        DecodeBuffer;

        if ErrorCounter >= MaxError then
        begin
          Connected:= false;
          COMState:= COMStopped;
        end;
      end;
    end;
  until Terminated;

  ser.Free;
end;

constructor TCOM.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate:= true;

  ser:= TBlockSerial.Create;
  ports:= TStringList.Create;
  LastFrame:= TFrame.Create;

  Connected:= false;
  COMState:= COMStopped;
  retryConnection:= false;
  ErrorCounter:= 0;

  inherited Create(CreateSuspended);
end;

destructor TCOM.Free;
begin
  LastFrame.Free;
  ports.Free;
  ser.Free;
end;

function TCOM.isConneted: boolean;
begin
  Result:= Connected;
end;

function TCOM.isTrying: boolean;
begin
  Result:= COMState = COMTrial;
end;

procedure TCOM.CMDPing;
begin
  ser.SendString('NeoBMS ping' + #10);
end;

procedure TCOM.CMDStdChg;
begin
  ser.SendString('NeoBMS std' + #10);
end;

procedure TCOM.CMDFullChg;
begin
  ser.SendString('NeoBMS full' + #10);
end;

procedure TCOM.CMDStartChg;
begin
  ser.SendString('NeoBMS chst' + #10);
end;

function TCOM.open_port: boolean;
var
  i: integer;
begin
  Result:= false;  // not yet found

  ser.CloseSocket;  // clear any (half)open connections
  ser.ConvertLineEnd:= true;
  updateAvailablePorts;

  for i:= 0 to 0 do//ports.Count-1 do  // go through all available ports
  begin
    try
      ser.Connect('COM3');//ports.Strings[i]);
      if ser.LastError = 0 then
      begin
        ser.Config(115200, 8, 'N', SB1, false, false);
        ser.Purge;
        if check_device then  // ping them and watch if something returns
        begin
          Result:= true;  // if yes we are ready
          exit;
        end
        else
          ser.CloseSocket;  // else close socket and try next one
      end
      else
        ser.CloseSocket;
    finally
    end;
  end;
end;

procedure TCOM.updateAvailablePorts;
begin
  ports.Delimiter:= ',';
  ports.DelimitedText:= GetSerialPortNames;
end;

procedure TCOM.DecodeBuffer;
var
  strt, ende: integer;
  cc: byte;
  frame, buf: string;
begin
  if length(RecvBuffer) > 0 then
  begin
    strt:= Pos('---NeoBMS---' + AnsiChar(#10), RecvBuffer);
  end;
  repeat
    strt:= Pos('---NeoBMS---' + AnsiChar(#10), RecvBuffer);
    ende:= Pos('---End Frame---' + AnsiChar(#10), RecvBuffer);
    frame:= LeftStr(RecvBuffer, ende + 15);
    frame:= RightStr(frame, length(frame) - strt + 1);
    RecvBuffer:= RightStr(RecvBuffer, Length(RecvBuffer) - ende);
  until (ende = 0) or (strt = 0) or (ende > strt);

  if (strt > 0) and (ende > strt) then
  begin
    cc:= 0;
    strt:= Pos('CC: ', frame);
    if strt = 0 then
    begin
      inc(ErrorCounter);
      exit;
    end;

    ende:= strt + 4;
    buf:= '';
    while frame[ende] <> #10 do
    begin
      buf:= buf + frame[ende];
      inc(ende);
    end;

    for ende:= 1 to strt-1 do
      inc(cc, byte(frame[ende]));

    try
      ende:= StrToInt(buf);
    except
      on E: Exception do
      begin
        inc(ErrorCounter);
        exit;
      end;
    end;

    if ende <> cc then
    begin
      inc(ErrorCounter);
      exit;
    end;

    LastFrame.putFrameLines(frame);
    LastFrame.AnalyseFrame;

    if Assigned(frameReturn) then
      frameReturn(LastFrame);
  end;
end;

end.

