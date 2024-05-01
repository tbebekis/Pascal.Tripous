{$DEFINE DEBUG}     // do this here or you can define a -dDEBUG in Project Options/Other/Custom Options, i.e. in a build mode so you can set up a Debug with leakview and a Default build mode without it

program App;

{$mode objfpc}{$H+}

uses
  {$IFDEF DEBUG}
  SysUtils,
  {$ENDIF}

  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, frm_MainForm, frm_simpleform, o_App, frm_SortForm,
frm_StatusFilterForm, frm_RangeForm, frm_LocateLookupForm, frm_BlobForm, 
frm_FilterForm, Tripous, frm_XmlForm;

{$R *.res}

begin
  {$IFDEF DEBUG}
  // Assuming your build mode sets -dDEBUG in Project Options/Other when defining -gh
  // This avoids interference when running a production/default build without -gh

  // Set up -gh output for the Leakview package:

  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$ENDIF DEBUG}

  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);

  Application.Run;
end.

