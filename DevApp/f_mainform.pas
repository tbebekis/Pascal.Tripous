unit f_MainForm;

{$mode DELPHI}
{$H+}
{$WARN 5079 off : Unit "$1" is experimental}

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


  , LazFileUtils
  , DB
  , SQLDB, MSSQLConn
  , IBConnection

  ,Laz2_DOM
  , RTTIGrids
  //,laz2_XMLWrite
  //,Rtti
  , Tripous
  , Tripous.Data
  //,o_TestBed
  ;
{
Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, ExtCtrls;
}
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
    ,o_TestBed
    ;

const
  FileName = 'Data.XML';







{ TMainForm }

procedure TMainForm.DoCreate;
begin
  inherited DoCreate;
  btnTest.OnClick := Self.AnyClick;
end;

procedure TMainForm.DoDestroy;
begin
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
begin
  ConInfo := TSqlConnectionInfo.Create();
  ConInfo.ConnectionString := 'Type=MsSql; Host=localhost; Database=AxCon5; User=teo; Psw=1234';
end;

procedure TMainForm.Test();
begin
  TestMetastores();
  //TestDynArray();
end;





end.

