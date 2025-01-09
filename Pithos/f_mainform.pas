unit f_MainForm;

{$MODE DELPHI}{$H+}

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

  , Tripous
  , o_App
  ;

type
  { TMainForm }
  TMainForm = class(TForm)
    btnConnectionInsert: TToolButton;
    MainMenu: TMainMenu;
    mmoLog: TMemo;
    mnuFile: TMenuItem;
    mnuExit: TMenuItem;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    pnlBody: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar: TStatusBar;
    ToolBar: TToolBar;
    btnExit: TToolButton;
    ToolBar1: TToolBar;
    btnConnectionEdit: TToolButton;
    btnConnectionDelete: TToolButton;
    btnConnectionISQL: TToolButton;
    btnConnectionSelectTable: TToolButton;
    tv: TTreeView;
  private
    IsInitialized: Boolean;
    procedure FormInitialize();
    procedure AnyClick(Sender: TObject);
    procedure ConnectionInsert();
    procedure ConnectionEdit();
    procedure ConnectionDelete();
  protected
    procedure DoShow; override;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  Tripous.Logs

  ,FileUtil

  ,f_ConnectionEditDialog
  ;

{ TMainForm }
procedure TMainForm.DoShow;
begin
  inherited DoShow;

  FormInitialize();
end;

procedure TMainForm.FormInitialize();
begin
  if not IsInitialized then
  begin
    IsInitialized := True;

    LogBox.Initialize(mmoLog, True);
    Application.ProcessMessages();

    App.AppInitialize();

    btnExit.OnClick  := AnyClick;
    btnConnectionInsert.OnClick  := AnyClick;
    btnConnectionEdit.OnClick  := AnyClick;
    btnConnectionDelete.OnClick  := AnyClick;
    btnConnectionISQL.OnClick  := AnyClick;
    btnConnectionSelectTable.OnClick  := AnyClick;

    btnConnectionEdit.Visible:= False;
  end;

end;

procedure TMainForm.AnyClick(Sender: TObject);
begin
  if btnExit = Sender then Close()
  else if btnConnectionInsert = Sender then ConnectionInsert()
  else if btnConnectionEdit = Sender then ConnectionEdit()
  else if btnConnectionDelete = Sender then ConnectionDelete()
  ;
end;

procedure TMainForm.ConnectionInsert();
begin
  TConnectionEditDialog.ShowDialog(TConInfoProxy.Create());
end;

procedure TMainForm.ConnectionEdit();
begin

end;

procedure TMainForm.ConnectionDelete();
begin

end;




end.

