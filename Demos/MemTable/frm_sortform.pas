unit frm_SortForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
  , SysUtils
  , Forms
  , Controls
  , Graphics
  , Dialogs
  , StdCtrls
  , ExtCtrls
  , DB
  , DBCtrls
  , DBGrids

  ,O_App
  ,Tripous.MemTable
  ;

type

  { TSortForm }

  TSortForm = class(TForm)
    Grid: TDBGrid;
    Label1: TLabel;
    Panel1: TPanel;
  private
    DS: TDatasource;
    Table: TMemTable;
    procedure InitializeTest();
    procedure Grid_TitleClick(Column: TColumn);
  protected
    procedure KeyPress(var Key: char); override;
    procedure DoShow; override;
  end;



implementation

{$R *.lfm}



procedure TSortForm.InitializeTest();
var
  i: Integer;
begin
  Label1.Caption := 'Click on a column caption to sort the column' + #10 + 'Cycles through Asc, Desc and None';

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

  for i := 0 to 2 do
  begin
    Table.Append();
    Table.FieldByName('Name').AsString := 'Name_' + IntToStr(i + 1);
    Table.Post();
  end;

  Grid.OnTitleClick := @Grid_TitleClick;
end;

procedure TSortForm.Grid_TitleClick(Column: TColumn);
begin
  Table.NextSort(Column.FieldName);
end;

procedure TSortForm.KeyPress(var Key: char);
begin
  if Key = #27 then
  begin
    Key := #0;
    Close;
  end;

  inherited KeyPress(Key);
end;

procedure TSortForm.DoShow;
begin
  inherited DoShow;
  KeyPreview := True;
  Position := poMainFormCenter;

  InitializeTest();
end;

end.

