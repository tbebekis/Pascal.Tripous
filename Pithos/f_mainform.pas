unit f_MainForm;

{$MODE DELPHI}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
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
  , LCLType

  , Tripous
  , Tripous.Data
  , o_App
  ;

type
  { TMainForm }
  TMainForm = class(TForm)
    btnInsertDatabase: TToolButton;
    ImageListMainForm: TImageList;
    ImageListInspector: TImageList;
    ImageListTreeView: TImageList;
    MainMenu: TMainMenu;
    mnuCollapseAll: TMenuItem;
    mnuSelectTableOrView: TMenuItem;
    mnuInsertDatabase: TMenuItem;
    mnuRemoveDatabase: TMenuItem;
    mnuReloadDatabase: TMenuItem;
    mnuISql: TMenuItem;
    mnuShowFieldList: TMenuItem;
    mnuShowMetadata: TMenuItem;
    Separator5: TMenuItem;
    Separator4: TMenuItem;
    Separator3: TMenuItem;
    Separator2: TMenuItem;
    Separator1: TMenuItem;
    mmoLog: TMemo;
    mnuFile: TMenuItem;
    mnuExit: TMenuItem;
    Pager: TPageControl;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    pnlBody: TPanel;
    mnuTV: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ToolBar: TToolBar;
    btnExit: TToolButton;
    ToolBar1: TToolBar;
    btnEditDatabase: TToolButton;
    btnRemoveDatabase: TToolButton;
    btnISql: TToolButton;
    btnCollapseAll: TToolButton;
    btnReloadDatabase: TToolButton;
    ToolButton1: TToolButton;
    tv: TTreeView;
  private
    IsInitialized: Boolean;

    procedure FormInitialize();
    procedure AnyClick(Sender: TObject);
    procedure tv_DoubleClick(Sender: TObject);
    procedure Pager_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure InsertDatabase();
    procedure EditDatabase();
    procedure RemoveDatabase();
  protected
    procedure DoShow; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
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

procedure TMainForm.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_F5) and (Shift = []) then
  begin
     if ActiveControl = tv then
        btnReloadDatabase.Click();
  end;

  inherited KeyDown(Key, Shift);
end;

procedure TMainForm.FormInitialize();
begin
  if not IsInitialized then
  begin
    IsInitialized := True;

    KeyPreview := True;

    Pager.Clear();

    LogBox.Initialize(mmoLog, True);
    Application.ProcessMessages();

    App.AppInitialize(tv, Pager);

    // buttons
    btnExit.OnClick  := AnyClick;
    btnISql.OnClick  := AnyClick;
    btnInsertDatabase.OnClick  := AnyClick;
    btnEditDatabase.OnClick  := AnyClick;
    btnRemoveDatabase.OnClick  := AnyClick;
    btnReloadDatabase.OnClick  := AnyClick;
    btnCollapseAll.OnClick  := AnyClick;

    // menus
    mnuISql.OnClick  := AnyClick;
    mnuInsertDatabase.OnClick  := AnyClick;
    mnuRemoveDatabase.OnClick  := AnyClick;
    mnuReloadDatabase.OnClick  := AnyClick;
    mnuCollapseAll.OnClick  := AnyClick;
    mnuSelectTableOrView.OnClick  := AnyClick;
    mnuShowFieldList.OnClick  := AnyClick;
    mnuShowMetadata.OnClick  := AnyClick;

    btnEditDatabase.Visible:= False;
    btnCollapseAll.Visible:= False;

    tv.OnDblClick := tv_DoubleClick;
    Pager.OnMouseDown := Pager_MouseDown;
  end;
end;

procedure TMainForm.AnyClick(Sender: TObject);
begin
  if btnExit = Sender then Close()
  else if (btnInsertDatabase = Sender) or (mnuInsertDatabase = Sender) then InsertDatabase()
  else if (btnEditDatabase = Sender) then EditDatabase()
  else if (btnRemoveDatabase = Sender) or (mnuRemoveDatabase = Sender)  then RemoveDatabase()
  else if (btnCollapseAll = Sender) or (mnuCollapseAll = Sender) then tv.FullCollapse()
  else if (btnReloadDatabase = Sender) or (mnuReloadDatabase = Sender) then App.ReloadSelectedDatabase()
  else if (btnISql = Sender) or (mnuISql = Sender) then App.AddSqlPage()
  else if mnuSelectTableOrView = Sender then App.SelectTableOrView()
  else if mnuShowFieldList = Sender then App.AddFieldListPage()
  else if mnuShowMetadata = Sender then App.AddMetadataPage()
  ;
end;

procedure TMainForm.tv_DoubleClick(Sender: TObject);
begin
  App.ReloadSelectedDatabase();
end;

procedure TMainForm.Pager_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index : Integer;
  R     : TRect;
  P     : TPoint;
begin
  if (Button = mbMiddle) and Assigned(Pager.ActivePage) then
  begin
    P.X := X;
    P.Y := Y;
    Index := Pager.ActivePage.PageIndex;
    R     := Pager.TabRect(Index);
    if R.Contains(P) then
      Pager.ActivePage.Free();
  end;
end;

procedure TMainForm.InsertDatabase();
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

procedure TMainForm.EditDatabase();
begin
end;

procedure TMainForm.RemoveDatabase();
begin
end;




end.

