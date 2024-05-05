unit frm_CalcField;

{$mode ObjFPC}{$H+}

interface


uses
  Classes
  , SysUtils
  , Forms
  , Controls
  , ExtCtrls
  , ComCtrls
  , Graphics
  , Dialogs
  , DB
  , DBCtrls
  , DBGrids

  ,TypInfo
  ,Tripous.MemTable
  ,o_App
  ;

type

  { TCalcFieldForm }
  TCalcFieldForm = class(TForm)
    Grid: TDBGrid;
  private
    Table : TMemTable;
    DS: TDatasource;
    procedure InitializeTest();
    procedure OnCalcFields(DataSet: TDataSet);
  protected
    procedure KeyPress(var Key: char); override;
    procedure DoShow; override;
  end;

var
  CalcFieldForm: TCalcFieldForm;

implementation

{$R *.lfm}

{ TCalcFieldForm }

procedure TCalcFieldForm.InitializeTest();
begin
  Table := TMemTable.Create(Self);

  Table.FieldDefs.AddString('FirstName', 16);
  Table.FieldDefs.AddString('LastName', 16);
  Table.CreateDataset();

  Table.AddCalcField('FullName', ftWideString, 100);
  Table.OnCalcFields := Addr(OnCalcFields);

  Table.Active := True;

  DS := TDataSource.Create(Self);
  DS.DataSet := Table;

  Grid.DataSource := DS;

  App.AdjustGridColumns(Grid);
end;

procedure TCalcFieldForm.OnCalcFields(DataSet: TDataSet);
begin
  Table.FieldByName('FullName').AsString := Table.FieldByName('FirstName').AsString + ' ' + Table.FieldByName('LastName').AsString;
end;

procedure TCalcFieldForm.KeyPress(var Key: char);
begin
  if Key = #27 then
  begin
    Key := #0;
    Close;
  end;

  inherited KeyPress(Key);
end;

procedure TCalcFieldForm.DoShow;
begin
  inherited DoShow;
  KeyPreview := True;
  Position := poMainFormCenter;

  InitializeTest();
end;

end.

