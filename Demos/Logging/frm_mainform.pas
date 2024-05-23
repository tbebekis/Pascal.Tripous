unit frm_MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls
  ,Tripous
  ,Tripous.Logs
  ;

type

  { TMainForm }
  TMainForm = class(TForm)
    btnLogInfo: TButton;
    btnLogError: TButton;
    btnLogSource: TButton;
    btnLogSourceAndScope: TButton;
    edtInfoText: TEdit;
    lblLogFolder: TLabel;
    mmoLog: TMemo;
    Panel1: TPanel;
  private
    FLogListener: TLogListener;
    FFileLogListener: TFileLogListener;


    procedure AnyClick(Sender: TObject);
    function GetLogText(): string;

    procedure LogProc(LogLine: string);

    procedure LogInfoTest();
    procedure LogErrorTest();
    procedure LogSourceTest();
    procedure LogSourceAndScopeTest();

    procedure Test();
    procedure Test2();
  protected
    procedure KeyPress(var Key: char); override;
    procedure DoShow; override;
    procedure DoDestroy; override;
  end;

var
  MainForm: TMainForm;

implementation

uses
  Variants
  ;

{$R *.lfm}

{ TMainForm }
procedure TMainForm.DoShow;
begin
  inherited DoShow;

  mmoLog.Clear();
  KeyPreview := True;

  btnLogInfo.OnClick  := Addr(AnyClick);
  btnLogError.OnClick := Addr(AnyClick);
  btnLogSource.OnClick := Addr(AnyClick);
  btnLogSourceAndScope.OnClick := Addr(AnyClick);


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

procedure TMainForm.KeyPress(var Key: char);
begin
  if Key = #27 then
  begin
    Key := #0;
    Close;
  end;

  inherited KeyPress(Key);
end;

procedure TMainForm.AnyClick(Sender: TObject);
begin
  if btnLogInfo = Sender then
    LogInfoTest()
  else if btnLogError = Sender then
    LogErrorTest()
  else if btnLogSource = Sender then
    LogSourceTest()
  else if btnLogSourceAndScope = Sender then
    LogSourceAndScopeTest()
  ;
end;

function TMainForm.GetLogText(): string;
begin
  if Length(edtInfoText.Text) > 0 then
    Result := edtInfoText.Text
  else
    Result := 'No log message is provided';
end;

procedure TMainForm.LogProc(LogLine: string);
begin
  mmoLog.Lines.Add(LogLine);
end;

procedure TMainForm.LogInfoTest();
begin
  Logger.Info(Self.ClassName, GetLogText());
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

procedure TMainForm.LogSourceTest();
var
  LogSource: ILogSource;
  Params: IVariantDictionary;
begin
  // simple
  LogSource := Logger.CreateLogSource(Self.ClassName);
  LogSource.Info(GetLogText());

  // with params
  Params := TVariantDictionary.Create();
  Params['CustomerId'] := 'BigCo';
  Params['OrderId']    := 123;

  LogSource.Log(loInfo, 'Customer {CustomerId} Order with {OrderId} is completed.', Params);
end;

procedure TMainForm.LogSourceAndScopeTest();
var
  LogSource: ILogSource;
begin
  LogSource := Logger.CreateLogSource(Self.ClassName);

  LogSource.EnterScope('Scope 2');
  LogSource.Info(GetLogText());

  LogSource.EnterScope('Scope 3');
  LogSource.Info(GetLogText());
  LogSource.ExitScope();

  LogSource.Info(GetLogText());
  LogSource.ExitScope();

  LogSource.Info(GetLogText());
end;

procedure TMainForm.Test();
var
  Dic: IVariantDictionary;
  Pair: TKeyValue;
begin
  Dic := TVariantDictionary.Create();
  Dic['Name'] := 'Pascal';
  Dic['Amount'] := 123;

  for Pair in Dic do
    LogProc(Format('%s = %s', [Pair.Key, VarToStr(Pair.Value)]));

  //Dic.Free();
end;

procedure TMainForm.Test2();
var
  Dic: IVariantDictionary;
  S : string;
begin
  // 'Customer {CustomerId} order {OrderId} is completed.'

  S := 'Customer {CustomerId} order {OrderId} is completed.'  ;

  Dic := TVariantDictionary.Create();
  Dic['CustomerId'] := 'Pascal';
  Dic['OrderId'] := 123;

  S := Logger.FormatParams(S, Dic);

  LogProc(S);
end;









end.

