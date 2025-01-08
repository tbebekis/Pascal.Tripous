unit f_MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes
  , SysUtils
  , Forms
  , Controls
  , Graphics
  , Dialogs
  , ComCtrls
  , Menus
  , ExtCtrls
  , StdCtrls
  ;

type
  { TMainForm }
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    mmoLog: TMemo;
    mnuFile: TMenuItem;
    mnuExit: TMenuItem;
    pnlRight: TPanel;
    pnlBody: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar: TStatusBar;
    ToolBar: TToolBar;
    btnExit: TToolButton;
    TreeView1: TTreeView;
  private

  protected
    procedure DoCreate; override;
    procedure DoDestroy; override;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  Tripous.Logs
  ;

{ TMainForm }

procedure TMainForm.DoCreate;
begin
  inherited DoCreate;
  LogBox.Initialize(mmoLog, True);
  LogBox.AppendLine('Hi there');
end;

procedure TMainForm.DoDestroy;
begin
  inherited DoDestroy;
end;

end.

