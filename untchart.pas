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
    chsVoltages: TListChartSource;
  private

  public
    procedure updateChart(frame: TFrame);
  end;

var
  frmChart: TfrmChart;

implementation

{$R *.lfm}

{ TfrmChart }

procedure TfrmChart.updateChart(frame: TFrame);
var
  i, cellcount: integer;
begin
  cellcount:= frame.getCellCount;

  if cellcount <> chsVoltages.Count then
  begin
    chsVoltages.Clear;
    for i:= 0 to cellcount-1 do
      chsVoltages.Add(i, 0);
  end;

  for i:= 0 to cellcount-1 do
    chsVoltages.SetYValue(i, frame.getCellU(i));
end;

end.

