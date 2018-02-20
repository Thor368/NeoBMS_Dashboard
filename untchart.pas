unit untChart;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TASources, TAGraph, TASeries, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, untCOM;

type

  { TfrmChart }

  TfrmChart = class(TForm)
    chrVoltages: TChart;
    cbsVoltages: TBarSeries;
    procedure chrVoltagesDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure updateChart(frame: TFrame);
  end;

var
  frmChart: TfrmChart;

implementation

{$R *.lfm}

{ TfrmChart }

procedure TfrmChart.FormCreate(Sender: TObject);
var
  i: integer;
begin
{  cbsVoltages.Clear;

  Randomize;
  for i:= 0 to 20 do
    cbsVoltages.AddXY(i, Random*1.7+2.5);
}
end;

procedure TfrmChart.chrVoltagesDblClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmChart.updateChart(frame: TFrame);
var
  i, cellcount: integer;
begin
  cellcount:= frame.getCellCount;

  if cellcount <> cbsVoltages.Count then
  begin
    cbsVoltages.Clear;
    for i:= 0 to cellcount-1 do
      cbsVoltages.AddXY(i, 0);
  end;

  for i:= 0 to cellcount-1 do
    cbsVoltages.SetYValue(i, frame.getCellU(i));
end;

end.

