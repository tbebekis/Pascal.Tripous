unit frm_SimpleForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
  , SysUtils
  , Forms
  , Controls
  , Graphics
  , Dialogs
  , DB
  , DBCtrls
  , DBGrids, ExtCtrls, StdCtrls

  ,Tripous.MemTable
  ,O_App
  ;

type

  { TSimpleForm }

  TSimpleForm = class(TForm)
    btnGetBookmark: TButton;
    btnGoToBookmark: TButton;
    Grid: TDBGrid;
    Panel1: TPanel;
  private
    DS: TDatasource;
    Table: TMemTable;

    FBookmark : TBookMark;
    procedure AnyClick(Sender: TObject);
    procedure InitializeTest();
  protected
    procedure KeyPress(var Key: char); override;
    procedure DoShow; override;
  end;

var
  SimpleForm: TSimpleForm;

implementation

{$R *.lfm}


{ TSimpleForm }

procedure TSimpleForm.AnyClick(Sender: TObject);
begin
  if btnGetBookmark = Sender then
     FBookmark := Table.GetBookmark
  else if btnGoToBookmark = Sender then
  begin
    if Table.BookmarkValid(FBookmark) then
      Table.GotoBookmark(FBookmark);
  end;
end;

procedure TSimpleForm.InitializeTest();
var
  i: Integer;
begin
  btnGetBookmark.OnClick := Addr(AnyClick);
  btnGoToBookmark.OnClick := Addr(AnyClick);

  // master
  Table := TMemTable.Create(Self);
  Table.FieldDefs.Add('Id', ftAutoInc);
  Table.FieldDefs.Add('Name', ftString, 100);
  //Table.FieldDefs.Add('Data', ftString, 100);
  Table.CreateDataset;

  DS := TDataSource.Create(Self);
  DS.DataSet := Table;
  Grid.DataSource := DS;

  Table.Active := True;

  App.AdjustGridColumns(Grid);

  Table.FieldByName('Id').ReadOnly := True;

  for i := 0 to 9 do
  begin
    Table.Append();
    Table.FieldByName('Name').AsString := 'Name_' + IntToStr(i + 1);
    Table.Post();
  end;
end;

procedure TSimpleForm.KeyPress(var Key: char);
begin
  if Key = #27 then
  begin
    Key := #0;
    Close;
  end;

  inherited KeyPress(Key);
end;

procedure TSimpleForm.DoShow;
begin
  inherited DoShow;
  KeyPreview := True;
  Position := poMainFormCenter;

  InitializeTest();
end;

end.

