unit untSearch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, untCOM, untDash;

type

  { TfrmSearch }

  TfrmSearch = class(TForm)
    lblConnectStatus: TLabel;
    tmrConnect: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lblConnectStatusClick(Sender: TObject);
    procedure tmrConnectTimer(Sender: TObject);
  protected
    pingTimer: integer;
  public
    procedure setSearching;
    procedure setWaiting;
    procedure setFound;

    procedure frameCallback(frame: TFrame);
  end;

var
  frmSearch: TfrmSearch;

implementation

{$R *.lfm}

{ TfrmSearch }

procedure TfrmSearch.FormCreate(Sender: TObject);
begin
  COM:= TCOM.Create(false);
  COM.retryConnection:= true;
  COM.frameReturn:= @frameCallback;
  pingTimer:= 0;
end;

procedure TfrmSearch.FormDestroy(Sender: TObject);
begin
  COM.Terminate;
end;

procedure TfrmSearch.lblConnectStatusClick(Sender: TObject);
begin
  if not COM.isConneted and not COM.isTrying then
    COM.retryConnection:= true;
end;

procedure TfrmSearch.tmrConnectTimer(Sender: TObject);
begin
  if COM.isConneted then
  begin
    setFound;

    if not frmDash.Visible then
      frmDash.Show;

    if pingTimer >= 10 then
    begin
      COM.CMDPing;
      pingTimer:= 0;
    end;
    inc(pingTimer);
  end
  else if COM.isTrying then
    setSearching
  else
    setWaiting;

  if not frmSearch.Visible and not frmDash.Visible then
    Close;
end;

procedure TfrmSearch.setSearching;
begin
  lblConnectStatus.Caption:= 'Searching for Device';
  lblConnectStatus.Color:= clYellow;
end;

procedure TfrmSearch.setWaiting;
begin
  lblConnectStatus.Caption:= 'Non Device' + #13#10 + '(click to search again)';
  lblConnectStatus.Color:= clRed;
end;

procedure TfrmSearch.setFound;
begin
  lblConnectStatus.Caption:= 'Device Found';
  lblConnectStatus.Color:= clLime;
end;

procedure TfrmSearch.frameCallback(frame: TFrame);
begin
  frmDash.updateDash(frame);
end;

end.

