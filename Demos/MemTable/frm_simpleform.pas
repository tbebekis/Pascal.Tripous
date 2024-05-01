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
  , DBGrids

  ,Tripous.MemTable
  ,O_App
  ;

type

  { TSimpleForm }

  TSimpleForm = class(TForm)
    Grid: TDBGrid;
  private
    DS: TDatasource;
    Table: TMemTable;
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

procedure TSimpleForm.InitializeTest();
var
  i: Integer;
begin
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

