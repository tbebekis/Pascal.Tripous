unit Tripous.Ui;

{$MODE DELPHI}{$H+}

interface

uses
  Classes
  , SysUtils
  , Forms
  , Controls
  , Graphics
  , Dialogs
  , ComCtrls
  , Menus
  , ExtCtrls
  //, StdCtrls
  , LCLType

  , Tripous
  , Tripous.Data
  , Tripous.Logs

  ;


type

  { Ui }
  Ui = class
  public
    class function  YesNoBox(Message: string): Boolean;
    class procedure InfoBox(Message: string);
    class procedure ErrorBox(Message: string);
  end;

implementation

{ Ui }
class function Ui.YesNoBox(Message: string): Boolean;
begin
  Result := MessageDlg('Question', Message, TMsgDlgType.mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

class procedure Ui.InfoBox(Message: string);
begin
  MessageDlg('Information', Message, TMsgDlgType.mtInformation, [mbOK], 0);
end;

class procedure Ui.ErrorBox(Message: string);
begin
  MessageDlg('Error', Message, TMsgDlgType.mtError, [mbOK], 0);
end;

end.

