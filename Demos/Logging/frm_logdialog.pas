unit frm_LogDialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, DBGrids, DBCtrls, ComCtrls;

//====================================================================
// this form is used in designing the TFormLogListener form
//====================================================================
type

  { TLogDialog }
  TLogDialog = class(TForm)
    btnClearLog: TButton;
    btnClearTable: TButton;
    Grid: TDBGrid;
    mmoLog: TMemo;
    mmoText: TDBMemo;
    Pager: TPageControl;
    pnlLog: TPanel;
    pnlTable: TPanel;
    Splitter: TSplitter;
    tabLog: TTabSheet;
    tabTable: TTabSheet;
  private

  public

  end;

var
  LogDialog: TLogDialog;

implementation

{$R *.lfm}

{ TLogDialog }



end.

