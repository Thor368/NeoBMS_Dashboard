program proNeoDash;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, untSearch, untCOM, untDash, untCMD, untChart, tachartlazaruspkg
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfrmSearch, frmSearch);
  Application.CreateForm(TfrmDash, frmDash);
  Application.CreateForm(TfrmCMD, frmCMD);
  Application.CreateForm(TfrmChart, frmChart);
  Application.Run;
end.

