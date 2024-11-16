unit f_MainForm;

{$mode DELPHI}
{$H+}


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
  , DB, SQLDB, IBConnection
  , Generics.Collections
  //, csvdataset
  ,Laz2_DOM, RTTIGrids
  //,laz2_XMLWrite
  ,Rtti


  ,Tripous.Generics
  ,o_TestBed
  ;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnTest: TButton;
    mmoLog: TMemo;
    SQLConnector1: TSQLConnector;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
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

uses
    fpjson
    ,jsonscanner

    //,jsonparser
    ,fpjsonrtti
    ;

const
  FileName = 'Data.XML';







{ TMainForm }

procedure TMainForm.DoCreate;
begin
  inherited DoCreate;

  //BufTableCreate();
  btnTest.OnClick := Self.AnyClick;
end;

procedure TMainForm.DoDestroy;
begin
  //BufTableDestroy();
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

  ShowMessage('asdf');
  //dsGrid.DataSet := tblPsw;
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



procedure TestConnInfo();
var
  ConInfo: TSqlConnectionInfo;
  S : string;
begin
  ConInfo := TSqlConnectionInfo.Create();
  ConInfo.ConnectionString := 'Type=MsSql; Host=localhost; Database=AxCon5; User=teo; Psw=1234';

  S := ConInfo.Provider;
end;

procedure TestArrayOfConst(V: string; A: array of const);
var
  i : Integer;
  S : string;
  Obj: TObject;
begin
  for i := 0 to High(A) do
  begin
    if A[i].VType = vtObject then
      Obj := A[i].VObject;
  end;
end;
function UnQuote(const S: string): string;
const
  A: array of char = ['''', '"', '[', ']', ' ', #9, #10, #11, #12, #13];
var
  i : Integer;
begin
   ShowMessage(S);

   Result := S.Trim(A);
   ShowMessage(Result);

   {
   for i := Low(A) to High(A) do
   begin
     if Result.StartsWith(A[i]) then
        Result := Result.Remove(0, 1);
     if Result.EndsWith(A[i]) then
        Result := Result.Remove(Length(S) - 1, 1);
   end;
   }

   //Result := Result.Trim();

   ShowMessage(Result);
end;

type

  { TPerson }

  TPerson = class
  public
    Name: string;
    constructor Create(AName: string);
    destructor Destroy(); override;
  end;

{ TPerson }

constructor TPerson.Create(AName: string);
begin
  Name := AName;
end;

destructor TPerson.Destroy();
begin
  inherited Destroy();
end;

function MatchPerson(Item: TPerson): Boolean;
begin
  Result := Sys.IsSameText(Item.Name, 'john');
end;

function ComparePerson(constref A, B: TPerson): Integer;
begin
  Result := AnsiCompareText(A.Name, B.Name);
end;

procedure TestGeneric_0();
var
  P1, P2, P3, P, Item : TPerson;
  PersonList: TGenObjectList<TPerson>;
  Count: Integer;
  S : string;
begin
  P1 := TPerson.Create('teo');
  P2 := TPerson.Create('john');
  P3 := TPerson.Create('mike');

  PersonList := TGenObjectList<TPerson>.Create(True, False);
  PersonList.Add(P1);
  PersonList.Add(P2);
  PersonList.Add(P3);

  Count := PersonList.Count;

  P := PersonList.FirstOrNil(MatchPerson);
  PersonList.Sort(ComparePerson);

  for Item in PersonList do
    S := Item.Name;

  PersonList.Free();
end;



procedure TMainForm.Test();
var
  S : string;
begin
  //TestMetastores();
  //S := ''''' ab         ]';
  //S := UnQuote(S);

  TestGeneric_0();

end;





end.

