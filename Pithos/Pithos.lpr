program Pithos;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, f_MainForm, Tripous.Crypt, Tripous.Data, Tripous.FilterParser,
  Tripous.Logs, Tripous.MemTable, Tripous, o_App,
  o_Temp, Tripous.Ui,
  f_BaseForm, f_SqlEditorForm, f_ISqlForm;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

