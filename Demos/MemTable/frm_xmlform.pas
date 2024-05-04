unit frm_XmlForm;

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

  { TXmlForm }

  TXmlForm = class(TForm)
    btnInitializeData: TButton;
    btnToXmlText: TButton;
    btnFromXmlText: TButton;
    edtRowCount: TEdit;
    Grid: TDBGrid;
    Label1: TLabel;
    mmoLog: TMemo;
    Pager: TPageControl;
    Panel1: TPanel;
    tabGrid: TTabSheet;
    tabLog: TTabSheet;
  private
    DS: TDatasource;
    Table: TMemTable;

    procedure AnyClick(Sender: TObject);

    procedure InitializeTest();
    procedure InitializeData();

    procedure ToXmlText();
    procedure FromXmlText();

    procedure SaveToFile();
    procedure LoadFromFile();
  protected
    procedure KeyPress(var Key: char); override;
    procedure DoShow; override;
  end;

var
  XmlForm: TXmlForm;

implementation

{$R *.lfm}

uses
  FmtBCD
  ;

{ TXmlForm }

procedure TXmlForm.InitializeTest();
begin
  Pager.ActivePage := tabGrid;

  InitializeData();

  btnInitializeData.OnClick := @AnyClick;
  btnToXmlText.OnClick      := @AnyClick;
  btnFromXmlText.OnClick    := @AnyClick;
end;



procedure TXmlForm.InitializeData();
var
  i : Integer;
  RowCount : Integer;
  //ImageFile: string;
  //Images : array of string = ('Laz1.png', 'Laz2.png', 'Laz3.png');
begin
  i := 1;

  FreeAndNil(DS);
  FreeAndNil(Table);

  RowCount := StrToInt(edtRowCount.Text);

  Randomize();

  Table := TMemTable.Create(Self);

  //Table.FieldDefs.Add('AUTOINC', ftAutoInc);
  Table.FieldDefs.AddString('STRING', 10);     // Table.FieldDefs.Add('STRING', ftWideString, 10, True);
  Table.FieldDefs.Add('GUID', ftGuid, 38);
  Table.FieldDefs.Add('INTEGER', ftInteger);
  Table.FieldDefs.Add('LARGE_INT', ftLargeint);
  Table.FieldDefs.Add('BOOL', ftBoolean);
  Table.FieldDefs.Add('FLOAT', ftFloat); //.Precision := 4;
  //Table.FieldDefs.Add('BCD', ftBCD);
  Table.FieldDefs.Add('CURRENCY', ftCurrency);
  Table.FieldDefs.Add('DATETIME', ftDateTime);
  //Table.FieldDefs.Add('GRAPHIC', ftGraphic);

  Table.CreateDataset;

  Table.Active := True;
  for i := 0 to RowCount - 1 do
  begin
    Table.Append();

    Table.FieldByName('STRING'        ).AsString := 'string_' + IntToStr(i);
    Table.FieldByName('GUID'          ).AsString := Table.NewGuid(False);
    Table.FieldByName('INTEGER'       ).AsInteger := i;
    if not ((i mod 3) = 0) then
      Table.FieldByName('LARGE_INT'     ).AsInteger := i * Random(1234);
    Table.FieldByName('BOOL'          ).AsBoolean := (i mod 2) = 0;
    Table.FieldByName('FLOAT'         ).AsFloat   := i * 0.32;
    Table.FieldByName('CURRENCY'      ).AsFloat   := i * 2.78;
    Table.FieldByName('DATETIME'      ).AsDateTime := Now();
    //Table.FieldByName('BCD'           ).AsBCD   := DoubleToBCD(i * 1.67);

    //ImageFile := Images[Random(3)];
    //TBlobField(Table.FieldByName('GRAPHIC')).LoadFromFile(ImageFile);

    Table.Post;
  end;

  DS := TDataSource.Create(Self);
  DS.DataSet := Table;
  Grid.DataSource := DS;

end;

procedure TXmlForm.ToXmlText();
var
  XmlText: string;
begin
  XmlText := TMemTable.ToXmlText(Table);
  mmoLog.Text := XmlText;

  Pager.ActivePage := tabLog;
end;

procedure TXmlForm.FromXmlText();
var
  XmlText: string;
begin
  DS.DataSet := nil;
  Table.Free();

  XmlText := mmoLog.Text;
  Table := TMemTable.FromXmlText(XmlText);
  Self.InsertComponent(Table);

  DS.DataSet := Table;
  Pager.ActivePage := tabGrid;
end;

procedure TXmlForm.SaveToFile();
begin

end;

procedure TXmlForm.LoadFromFile();
begin

end;

procedure TXmlForm.AnyClick(Sender: TObject);
begin
   if btnInitializeData = Sender then
     InitializeData()
   else if btnToXmlText = Sender then
     ToXmlText()
   else if btnFromXmlText = Sender then
     FromXmlText()
   ;
end;

procedure TXmlForm.KeyPress(var Key: char);
begin
  if Key = #27 then
  begin
    Key := #0;
    Close;
  end;

  inherited KeyPress(Key);
end;

procedure TXmlForm.DoShow;
begin
  inherited DoShow;
  KeyPreview := True;
  Position := poMainFormCenter;

  InitializeTest();
end;

end.

