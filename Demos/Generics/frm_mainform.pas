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
    edtGiantRange: TEdit;
    edtListWhere: TEdit;
    edtListNames: TEdit;
    edtListFirstOrDefault: TEdit;
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
end;

procedure TMainForm.AnyClick(Sender: TObject);
begin
  if btnListAddRange = Sender then
    GenListTest_AddRange(edtListNames.Text)
  else if btnListAddGiantRange = Sender then
    GenListTest_AddGiantRange(StrToInt(edtGiantRange.Text))
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
    ;
end;



end.

