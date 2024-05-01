unit frm_MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls

  ,Tripous.Logs
  ,LazLogger
  ;

type
  { TMainForm }
  TMainForm = class(TForm)
    btnLogInfo: TButton;
    btnLogError: TButton;
    edtInfoText: TEdit;
    lblLogFolder: TLabel;
    mmoLog: TMemo;
    Panel1: TPanel;
  private
    FLogListener: TLogListener;
    FFileLogListener: TFileLogListener;
    procedure AnyClick(Sender: TObject);

    procedure LogProc(LogLine: string);

    procedure LogInfoTest();
    procedure LogErrorTest();
  protected
    procedure KeyPress(var Key: char); override;
    procedure DoShow; override;
    procedure DoDestroy; override;
  end;

var
  MainForm: TMainForm;

implementation



{$R *.lfm}

{ TMainForm }

procedure TMainForm.AnyClick(Sender: TObject);
begin
  if btnLogInfo = Sender then
    LogInfoTest()
  else if btnLogError = Sender then
    LogErrorTest()
  ;
end;

procedure TMainForm.LogProc(LogLine: string);
begin
  mmoLog.Lines.Add(LogLine);
end;

procedure TMainForm.LogInfoTest();
var
  S : string;
begin
  if Length(edtInfoText.Text) > 0 then
    S := edtInfoText.Text
  else
    S := 'No log message is provided';

  Logger.Info(Self.ClassName, S);
end;

procedure TMainForm.LogErrorTest();
begin
  try
    raise Exception.Create('This is an Exception message');
  except
    on E: Exception do
      Logger.Error(Self.ClassName, E);
  end;
end;

procedure TMainForm.KeyPress(var Key: char);
begin
  if Key = #27 then
  begin
    Key := #0;
    Close;
  end;

  inherited KeyPress(Key);
end;

procedure TMainForm.DoShow;
begin
  inherited DoShow;

  mmoLog.Clear();
  KeyPreview := True;

  btnLogInfo.OnClick  := Addr(AnyClick);
  btnLogError.OnClick := Addr(AnyClick);

  Logger.Active   := True;
  Logger.MinLevel := TLogLevel.loDebug;

  { 1. A TLogListener automatically adds itself to Logger.Listeners when created
       and automatically removes itself from Logger.Listeners when destroyed.
    2. TLogTextListener and TLogLineListener inherit from TLogToMainThreadListener
       which synchronizes updates to MainThread controls  }
  FLogListener     := TLogTextListener.Create(Addr(LogProc));
  FFileLogListener := TFileLogListener.Create();

  lblLogFolder.Caption := Format('Error logs are saved at folder: %s ', [Logger.LogFolder]);
end;

procedure TMainForm.DoDestroy;
begin
  FreeAndNil(FFileLogListener);
  FreeAndNil(FLogListener);

  inherited DoDestroy;
end;

end.

