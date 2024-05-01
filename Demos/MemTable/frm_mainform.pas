unit frm_MainForm;

{$mode objfpc}{$H+}

interface


uses
  Classes
  , SysUtils
  , Forms
  , Controls
  , Graphics
  , Dialogs
  , StdCtrls
  ;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnBlobs: TButton;
    btnFilter: TButton;
    btnLocateLookup: TButton;
    btnMasterDetail: TButton;
    btnRange: TButton;
    btnSimple: TButton;
    btnSort: TButton;
    btnStatusFilter: TButton;
    btnXML: TButton;
  private
    procedure AnyClick(Sender: TObject);
    procedure ShowModalForm(FormClass: TFormClass);
  protected
    procedure KeyPress(var Key: char); override;
    procedure DoShow; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}


uses
   frm_SimpleForm
  ,frm_MasterDetailForm
  ,frm_SortForm
  ,frm_StatusFilterForm
  ,frm_RangeForm
  ,frm_LocateLookupForm
  ,frm_BlobForm
  ,frm_FilterForm
  ,frm_XmlForm
  ;

{ TMainForm }

procedure TMainForm.AnyClick(Sender: TObject);
begin
  if (btnSimple = Sender) then
      ShowModalForm(TSimpleForm)
  else if (btnMasterDetail = Sender) then
      ShowModalForm(TMasterDetailForm)
  else if (btnSort = Sender) then
      ShowModalForm(TSortForm)
  else if (btnStatusFilter = Sender) then
      ShowModalForm(TStatusFilterForm)
  else if (btnRange = Sender) then
      ShowModalForm(TRangeForm)
  else if (btnLocateLookup = Sender) then
      ShowModalForm(TLocateLookupForm)
  else if (btnBlobs = Sender) then
      ShowModalForm(TBlobForm)
  else if (btnFilter = Sender) then
         ShowModalForm(TFilterForm)
  else if (btnXML = Sender) then
         ShowModalForm(TXmlForm)
  ;
end;

procedure TMainForm.ShowModalForm(FormClass: TFormClass);
var
  F: TForm;
begin
  F := FormClass.Create(nil);
  try
    F.ShowModal();
  finally
    F.Free;
  end;
end;

procedure TMainForm.KeyPress(var Key: char);
begin
  if Key = #27 then
  begin
    Key := #0;
    Close;
  end;

  inherited KeyPress(Key);
end;

procedure TMainForm.DoShow;
begin
  inherited DoShow;

  KeyPreview := True;

  btnSimple.OnClick := @AnyClick;
  btnMasterDetail.OnClick := @AnyClick;
  btnSort.OnClick := @AnyClick;
  btnStatusFilter.OnClick := @AnyClick;
  btnRange.OnClick := @AnyClick;
  btnLocateLookup.OnClick := @AnyClick;
  btnBlobs.OnClick := @AnyClick;
  btnFilter.OnClick := @AnyClick;
  btnXML.OnClick := @AnyClick;
end;

end.

