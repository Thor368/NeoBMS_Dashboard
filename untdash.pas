unit untDash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, untCOM, untCMD, untChart;

type


  { TfrmDash }

  TfrmDash = class(TForm)
    cmdCMD: TButton;
    cmdUChart: TButton;
    cmdExit: TButton;
    Label1: TLabel;
    lblSOC: TLabel;
    lblTbat: TLabel;
    lblOV: TLabel;
    lblUmin: TLabel;
    lblUmax: TLabel;
    lblUabs: TLabel;
    lblDU: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lblIbat: TLabel;
    procedure cmdCMDClick(Sender: TObject);
    procedure cmdExitClick(Sender: TObject);
    procedure cmdUChartClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  protected

  public
    procedure updateDash(frame: TFrame);
  end;

var
  frmDash: TfrmDash;

implementation

{$R *.lfm}

{ TfrmDash }

procedure TfrmDash.FormCreate(Sender: TObject);
begin

end;

procedure TfrmDash.cmdExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmDash.cmdUChartClick(Sender: TObject);
begin
  frmChart.Show;
end;

procedure TfrmDash.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Application.Terminate;
end;

procedure TfrmDash.cmdCMDClick(Sender: TObject);
begin
  frmCMD.Show;
end;

procedure TfrmDash.updateDash(frame: TFrame);
begin
  lblIbat.Caption:= FloatToStrF(frame.getIbat, ffNumber, 3, 3) + 'A';
  lblSOC.Caption:= FloatToStrF(frame.getSOC, ffNumber, 0, 0) + '%';
  lblUmin.Caption:= FloatToStrF(frame.getUmin, ffNumber, 3, 3) + 'V';
  lblUmax.Caption:= FloatToStrF(frame.getUmax, ffNumber, 3, 3) + 'V';
  lblUabs.Caption:= FloatToStrF(frame.getUabs, ffNumber, 1, 1) + 'V';
  lblDU.Caption:= FloatToStrF(frame.getDU, ffNumber, 3, 3) + 'V';
  lblOV.Caption:= FloatToStrF(frame.getOV, ffNumber, 3, 3) + 'V';;

  frmChart.updateChart(frame);
end;

end.

