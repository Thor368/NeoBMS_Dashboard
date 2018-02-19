unit untCMD;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, untCOM;

type

  { TfrmCMD }

  TfrmCMD = class(TForm)
    cmdPing: TButton;
    cmdBack: TButton;
    cmdStdChg: TButton;
    cmdFullChg: TButton;
    cmdStartChg: TButton;
    procedure cmdBackClick(Sender: TObject);
    procedure cmdFullChgClick(Sender: TObject);
    procedure cmdPingClick(Sender: TObject);
    procedure cmdStartChgClick(Sender: TObject);
    procedure cmdStdChgClick(Sender: TObject);
  private

  public

  end;

var
  frmCMD: TfrmCMD;

implementation

{$R *.lfm}

{ TfrmCMD }

procedure TfrmCMD.cmdBackClick(Sender: TObject);
begin
  frmCMD.Hide;
end;

procedure TfrmCMD.cmdFullChgClick(Sender: TObject);
begin
  COM.CMDFullChg;
end;

procedure TfrmCMD.cmdPingClick(Sender: TObject);
begin
  COM.CMDPing;
end;

procedure TfrmCMD.cmdStartChgClick(Sender: TObject);
begin
  COM.CMDStartChg;
end;

procedure TfrmCMD.cmdStdChgClick(Sender: TObject);
begin
  COM.CMDStdChg;
end;

end.

