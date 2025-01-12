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
  , Tripous.Data, SQLDB
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
    SQLConnector1: TSQLConnector;
    StatusBar: TStatusBar;
    ToolBar: TToolBar;
    btnExit: TToolButton;
    ToolBar1: TToolBar;
    btnConnectionEdit: TToolButton;
    btnConnectionDelete: TToolButton;
    btnConnectionISQL: TToolButton;
    btnConnectionSelectTable: TToolButton;
    btnCollapse: TToolButton;
    btnReloadDatabase: TToolButton;
    tv: TTreeView;
  private
    IsInitialized: Boolean;

    procedure FormInitialize();
    procedure AnyClick(Sender: TObject);
    procedure tv_DoubleClick(Sender: TObject);
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

    App.AppInitialize(tv);

    btnExit.OnClick  := AnyClick;
    btnConnectionInsert.OnClick  := AnyClick;
    btnConnectionEdit.OnClick  := AnyClick;
    btnConnectionDelete.OnClick  := AnyClick;
    btnConnectionISQL.OnClick  := AnyClick;
    btnConnectionSelectTable.OnClick  := AnyClick;
    btnCollapse.OnClick  := AnyClick;
    btnReloadDatabase.OnClick  := AnyClick;

    btnConnectionEdit.Visible:= False;

    tv.OnDblClick := tv_DoubleClick;


  end;
end;

procedure TMainForm.AnyClick(Sender: TObject);
begin
  if btnExit = Sender then Close()
  else if btnConnectionInsert = Sender then ConnectionInsert()
  else if btnConnectionEdit = Sender then ConnectionEdit()
  else if btnConnectionDelete = Sender then ConnectionDelete()
  else if btnCollapse = Sender then tv.FullCollapse()
  else if btnReloadDatabase = Sender then App.ReloadSelectedDatabase()
  ;
end;

procedure TMainForm.tv_DoubleClick(Sender: TObject);
begin
  App.ReloadSelectedDatabase();
end;

procedure TMainForm.ConnectionInsert();
var
  ConInfoProxy: TSqlConInfoProxy;
  MetaDatabase: TMetaDatabase;
begin
  ConInfoProxy := TSqlConInfoProxy.Create();
  if TConnectionEditDialog.ShowDialog(ConInfoProxy) then
  begin
     MetaDatabase := App.ConnectionInsert(ConInfoProxy);
     App.AddDatabaseNode(MetaDatabase);
  end;
end;

procedure TMainForm.ConnectionEdit();
begin

end;

procedure TMainForm.ConnectionDelete();
begin

end;




end.

