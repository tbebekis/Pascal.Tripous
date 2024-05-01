unit f_MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes
  , SysUtils
  , FileUtil
  , Forms
  , Controls
  , Graphics
  , StdCtrls
  , Dialogs
  , Menus
  , DBGrids
  , TypInfo
  , Variants
  //, syncobjs
  , Tripous
  , Tripous.Data
  , LazFileUtils
  , DB
  //, Generics.Collections
  //, csvdataset
  ,Laz2_DOM, RTTIGrids
  //,laz2_XMLWrite

  ,o_TestBed
  ;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnTest: TButton;
    dsGrid: TDataSource;
    Grid: TDBGrid;
  private
    tblPsw: TBufTable;
    procedure BufTableCreate();
    procedure BufTableDestroy();

    procedure AnyClick(Sender: TObject);
    procedure Test();
  protected
    procedure DoCreate; override;
    procedure DoDestroy; override;
  public


  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

const
  FileName = 'Data.XML';

procedure XmlTest();
var
  Doc: TXMLDocument;
  Node: TDOMNode;
  S : string;
begin

    Doc := Xml.CreateDoc();
    Node := Xml.AddNode(Doc.DocumentElement, 'First', True);

    S := 'Ολα καλά' + LB + 'ki ola wraia';
    Xml.SetCData(Doc.DocumentElement, 'Second', S);

    //Xml.SetChild(Doc.DocumentElement, 'First', 'Paparia');
    //S := Xml.GetNodeValue(Node, '');
    //ShowMessage(S);

    //TDOMElement(Node)['FirstName'] := 'Θόδωρος';
    //S := TDOMElement(Node)['FirstName'];
    //ShowMessage(S);

    Xml.SaveToFile(Doc, 'Test.Xml');

    S := Xml.DocToText(Doc);
    ShowMessage(S);
end;

{ TMainForm }

procedure TMainForm.DoCreate;
begin
  inherited DoCreate;

  BufTableCreate();
  btnTest.OnClick := @Self.AnyClick;
end;

procedure TMainForm.DoDestroy;
begin
  BufTableDestroy();
  inherited DoDestroy;
end;

procedure TMainForm.BufTableCreate();
var
  FilePath: string;
begin
  tblPsw := TBufTable.Create(Self);

  FilePath := ExpandFileName(FileName);

  tblPsw.Close();
  if (FileExists(FilePath)) then
  begin
    //ShowMessage(Format('File exists: %s', [FilePath]));
    tblPsw.LoadFromFile(FilePath);
  end else
  begin
    //ShowMessage(Format('File NOT found: %s', [FilePath]));
    tblPsw.FieldDefs.Add('Service', ftString, 96);
    tblPsw.FieldDefs.Add('UserName', ftString, 96);
    tblPsw.FieldDefs.Add('Password', ftString, 96);
    tblPsw.FieldDefs.Add('Email', ftString, 96);
    tblPsw.FieldDefs.Add('Notes', ftMemo);

    //tblPsw.CreateDataset;
    tblPsw.Open();
  end;


  dsGrid.DataSet := tblPsw;
end;

procedure TMainForm.BufTableDestroy();
var
  FilePath: string;
begin
  if Assigned(tblPsw) then
  begin
    FilePath := ExpandFileName(FileName);

    if (tblPsw.Active) then
       tblPsw.SaveToFileXml(FilePath);
  end;
end;


procedure TMainForm.AnyClick(Sender: TObject);
begin
  if Sender = btnTest then
     Test();
end;

procedure TMainForm.Test;
var
  S : string;
  SL : TStringList;
begin
  SL := TStringList.Create();
  S := Rtti.UnitNameOf(TypeInfo(TAlign));

  //EnumLiteralByIndex(Info: PTypeInfo; Index: Integer): string;
  ShowMessage(S);

   XmlTest();
end;





end.

