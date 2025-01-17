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
  Tripous.Logs, Tripous.MemTable, Tripous, fr_ISqlFrame, o_App,
  f_ConnectionEditDialog, o_SqlHistory, o_Temp, fr_TextEditorFrame, Tripous.Ui;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

