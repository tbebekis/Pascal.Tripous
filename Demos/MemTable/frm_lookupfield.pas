unit frm_LookUpField;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
  , SysUtils
  , Forms
  , Controls
  , ExtCtrls
  , StdCtrls
  , ComCtrls
  , Graphics
  , Dialogs
  , DB
  , DBCtrls
  , DBGrids

  ,TypInfo
  ,Tripous.MemTable
  ;

type

  { TLookUpFieldForm }

  TLookUpFieldForm = class(TForm)
    gridCustomer: TDBGrid;
    gridCountry: TDBGrid;
    Splitter1: TSplitter;
  private
    tblCustomer: TMemTable;
    tblCountry: TMemTable;
    dsCustomer: TDatasource;
    dsCountry: TDatasource;

    procedure AnyClick(Sender: TObject);

    procedure InitializeTest();
  protected
    procedure KeyPress(var Key: char); override;
    procedure DoShow; override;
  end;

var
  LookUpFieldForm: TLookUpFieldForm;

implementation

{$R *.lfm}

{ TLookUpFieldForm }

procedure TLookUpFieldForm.AnyClick(Sender: TObject);
begin

end;

procedure TLookUpFieldForm.InitializeTest();
var
  //Images : array of string = ('Laz1.png', 'Laz2.png', 'Laz3.png');
  i : Integer;
  Countries : array of string = ('Greece', 'Italy', 'Spain');
  Customers : array of string = ('Good Foods Co.', 'Oils Co.', 'Great Bank', 'Constructions Co.');
begin
   tblCustomer := TMemTable.Create(Self);
   tblCustomer.FieldDefs.Add('Id', ftAutoInc);
   tblCustomer.FieldDefs.AddString('Name', 20);
   tblCustomer.FieldDefs.Add('CountryId', ftInteger);
   tblCustomer.CreateDataset();

   tblCountry := TMemTable.Create(Self);
   tblCountry.FieldDefs.Add('Id', ftAutoInc);
   tblCountry.FieldDefs.AddString('Name', 20);
   tblCountry.CreateDataset;

   tblCustomer.AddLookUpField('Customer', 'CountryId', tblCountry, 'Id', 'Name');

   tblCountry.Active := True;
   for i := Low(Countries) to High(Countries) do
   begin
     tblCountry.Append();
     tblCountry.FieldByName('Name').AsString := Countries[i];
     tblCountry.Post();
   end;


   Randomize();

   tblCustomer.Active := True;

   for i := Low(Customers) to High(Customers) do
   begin
     tblCustomer.Append();
     tblCustomer.FieldByName('Name').AsString := Customers[i];
     tblCustomer.FieldByName('CountryId').AsInteger := Random(tblCountry.RecordCount) + 1;
     tblCustomer.Post();
   end;

  dsCustomer := TDatasource.Create(Self);
  dsCountry  := TDatasource.Create(Self);

  dsCustomer.DataSet := tblCustomer;
  dsCountry.DataSet := tblCountry;

  gridCustomer.DataSource := dsCustomer;
  gridCountry.DataSource := dsCountry;
end;



procedure TLookUpFieldForm.KeyPress(var Key: char);
begin
  if Key = #27 then
  begin
    Key := #0;
    Close;
  end;

  inherited KeyPress(Key);
end;

procedure TLookUpFieldForm.DoShow;
begin
  inherited DoShow;
  KeyPreview := True;
  Position := poMainFormCenter;

  InitializeTest();
end;

end.

