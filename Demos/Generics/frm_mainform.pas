unit frm_MainForm;

{$MODE DELPHI}{$H+}

interface

uses
  Classes
  ,SysUtils
  ,Forms
  ,Controls
  ,Graphics
  ,Dialogs
  ,ExtCtrls
  ,StdCtrls, ComCtrls
  ,Tripous
  ,Tripous.Generics
  ;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnListAddRange: TButton;
    btnListAddGiantRange: TButton;
    btnListClear: TButton;
    btnListShow: TButton;
    btnListReverse: TButton;
    btnListSort: TButton;
    btnListFirstOrDefault: TButton;
    btnListWhere: TButton;
    btnDicClear: TButton;
    btnDicShow: TButton;
    btnDicAdd: TButton;
    btnDicAddEntries: TButton;
    edtDicKey: TEdit;
    edtDicValue: TEdit;
    edtListGiantRange: TEdit;
    edtDicAddEntries: TEdit;
    edtListWhere: TEdit;
    edtListNames: TEdit;
    edtListFirstOrDefault: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    mmoLog: TMemo;
    Pager: TPageControl;
    Panel1: TPanel;
    tabList: TTabSheet;
    tabDic: TTabSheet;
  private
     procedure AnyClick(Sender: TObject);
  protected
    procedure DoShow; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  o_Test
  ;

{ TMainForm }

procedure TMainForm.DoShow;
begin
  inherited DoShow;
  mmoLog.Clear();

  o_Test.mmoLog := Self.mmoLog;

  btnListAddRange.OnClick := AnyClick;
  btnListAddGiantRange.OnClick := AnyClick;
  btnListClear.OnClick := AnyClick;
  btnListShow.OnClick := AnyClick;
  btnListReverse.OnClick := AnyClick;
  btnListSort.OnClick := AnyClick;

  btnListFirstOrDefault.OnClick := AnyClick;
  btnListWhere.OnClick := AnyClick;

  btnDicClear.OnClick := AnyClick;
  btnDicShow.OnClick := AnyClick;
  btnDicAdd.OnClick := AnyClick;
  btnDicAddEntries.OnClick := AnyClick;

  Pager.ActivePage := tabList;

  GenListTest_Show();
end;

procedure TMainForm.AnyClick(Sender: TObject);
begin
  if btnListAddRange = Sender then
    GenListTest_AddRange(edtListNames.Text)
  else if btnListAddGiantRange = Sender then
    GenListTest_AddGiantRange(StrToInt(edtListGiantRange.Text))
  else if btnListClear = Sender then
    GenListTest_Clear()
  else if btnListShow = Sender then
    GenListTest_Show()
  else if btnListReverse = Sender then
    GenListTest_Reverse()
  else if btnListSort = Sender then
    GenListTest_Sort()
  else if btnListFirstOrDefault = Sender then
    GenListTest_FirstOrDefault(edtListFirstOrDefault.Text)
  else if btnListWhere = Sender then
    GenListTest_Where(edtListWhere.Text)

  else if btnDicClear  = Sender then
    DicTest_Clear()
  else if btnDicShow  = Sender then
    DicTest_Show()
  else if btnDicAdd  = Sender then
    DicTest_AddEntry(edtDicKey.Text, edtDicValue.Text)
  else if btnDicAddEntries  = Sender then
    DicTest_AddEntries(StrToInt(edtDicAddEntries.Text))
   ;
end;



end.

