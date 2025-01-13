unit o_SqlHistory;

{$MODE DELPHI}{$H+}
{$WARN 4104 off : Implicit string type conversion from "$1" to "$2"}
{$WARN 4105 off : Implicit string type conversion with potential data loss from "$1" to "$2"}
interface

uses
  Classes
  , SysUtils
  , Tripous
  ;

type

  { TSqlStatementItem }
  { Represents an Sql statement executed by the ISql form, for history (Prior-Next) purposes }
  TSqlStatementItem = class
  private
    FSqlText: string;
    FStatementName: string;
    function GetIsSelect: Boolean;
  public
    constructor Create(SqlText: string);

    property SqlText: string read FSqlText;
    property IsSelect: Boolean read GetIsSelect;
    property StatementName: string read FStatementName;
  end;

  { TSqlHistoryItem }
  { Represents a history item. A history item is created every time the user
    executes Sql in the ISqlForm. Since the form Sql editor may contain more than
    a single statement, the history item parses the editor text into SqlStatementItem items.  }
  TSqlHistoryItem = class
  private
    FSqlStatements: IList<TSqlStatementItem>;
    FSqlText: string;
  public
    constructor Create(SqlText: string);

    property SqlText: string read FSqlText;
    property SqlStatements: IList<TSqlStatementItem> read FSqlStatements;
  end;

  { TSqlHistory }
  { Controls the history items. }
  TSqlHistory = class
  private
    FItems: IList<TSqlHistoryItem>;
    FCurrentSqlText: string;
    FIndex: Integer;
    FCurrentSqlTextChanged: TNotifyEvent;
    function GetBof: Boolean;
    function GetCurrent: TSqlHistoryItem;
    function GetEof: Boolean;
    function GetCurrentSqlText: string;
    procedure SetCurrentSqlText(AValue: string);
  public
    constructor Create();

    {Called every time the user executes Sql statements.
    SqlText is the text of the Sql editor of the ISqlForm.
    The specified SqlText is used to create a HistoryItem which parses  the Text into SqlStatementItem items.}
    procedure Add(SqlText: string);
    { Advances an internal index into the next HistoryItem, if any, and assigns the CurrentSqlText property according to that HistoryItem. }
    procedure Next();
    { Sets an internal index to the prior HistoryItem, if any, and assigns the CurrentSqlText property according to that HistoryItem.}
    procedure Prior();

    { Returns true if the internal index is in the last HistoryItem   }
    property Eof: Boolean read GetEof;
    { Returns true if the internal index is in the first HistoryItem  }
    property Bof: Boolean read GetBof;
    { Gets the current HistoryItem, if any, else nil     }
    property Current: TSqlHistoryItem read GetCurrent;
    { Ges the Text of the Current HistoryItem, if any, else string.Empty    }
    property CurrentSqlText: string read GetCurrentSqlText;

    property CurrentSqlTextChanged: TNotifyEvent read FCurrentSqlTextChanged write FCurrentSqlTextChanged;
  end;

implementation

var
  StatementNames : array of string = ['select', 'execute', 'exec', 'insert', 'update', 'delete', 'create', 'alter', 'drop', 'truncate'];

{ TSqlStatementItem }
function TSqlStatementItem.GetIsSelect: Boolean;
begin
  Result := FStatementName = 'select';
end;

constructor TSqlStatementItem.Create(SqlText: string);
var
  sSqlText, Item: string;
begin
  inherited Create();
  FSqlText := SqlText;
  sSqlText := SqlText.Trim();
  for Item in StatementNames do
    if sSqlText.StartsWith(Item, True) then
    begin
      FStatementName := Item;
      break;
    end;
end;

{ TSqlHistoryItem }

constructor TSqlHistoryItem.Create(SqlText: string);
var
  SB: IStringBuilder;
  Lines: TStringArray;
  Line: string;

  TextList : IList<string>;
  SqlList  : IList<string>;
  LastLine : string;
  //S        : string;
  S2       : string;
  i        : Integer;


  StatementName: string;

  // -----------------------------------------------------------------------
  function GetStatementName(sLine: string): string;
  var
    sName: string;
  begin
    Result := '';

    sLine := sLine.Trim();
    if not Sys.IsEmpty(sLine) then
    begin
      for sName in StatementNames do
      begin
        if sLine.StartsWith(sName, True) then
        begin
          if sName = 'select' then
          begin
            if not ((Length(LastLine) > 0) and (LastLine[LastLine.Length - 1] = '('))  then
              Result := sName;
            break;
          end else begin
            Result := sName;
            break;
          end;
        end;
      end;
    end;
  end;
  // -----------------------------------------------------------------------
  procedure AddText(List: IList<string>; Text: string; StripEmptyLines: Boolean);
  var
    SplitOptions: TStringSplitOptions;
    Lines2: TStringArray;
    Item: string;
  begin
    if not Sys.IsEmpty(Text) then
    begin
      if StripEmptyLines then
        SplitOptions := TStringSplitOptions.ExcludeEmpty
      else
        SplitOptions := TStringSplitOptions.None;

        Lines2 := Text.Split([sLineBreak], SplitOptions);
        for Item in Lines2 do
        begin
          if not (Item.Trim().StartsWith('--')
               or Item.Trim().StartsWith('##')
               or Item.Trim().StartsWith('//')) then
          List.Add(Item);
        end;
    end;
  end;
  // -----------------------------------------------------------------------
begin
  inherited Create();
  FSqlText := SqlText;

  FSqlStatements := TGenObjectList<TSqlStatementItem>.Create(True, False);

  // strip empty lines
  SB := TStrBuilder.Create();
  Lines := SqlText.Trim().Split([sLineBreak], TStringSplitOptions.ExcludeEmpty);

  for Line in Lines do
  begin
    if not Sys.IsEmpty(Line) then
       SB.AppendLine(Line);
  end;

  SqlText := SB.ToString();

  // strip comments from Text
  TextList := TGenList<string>.Create();
  SqlList  := TGenList<string>.Create();
  LastLine := '';

  AddText(TextList, SqlText, True);

  for i := 0 to TextList.Count - 1 do
  begin
    Line := TextList[i].Trim();

    if not Sys.IsEmpty(Line) then
    begin
      StatementName := GetStatementName(Line);
      if not Sys.IsEmpty(StatementName) then
      begin
        S2 := Sys.ToText(SqlList);
        if not Sys.IsEmpty(S2) then
           SqlStatements.Add(TSqlStatementItem.Create(S2));
        SqlList.Clear();
      end;

      LastLine := Line;
      SqlList.Add(TextList[i]);
    end;

    {
    if not Sys.IsEmpty(S) then
    begin
      if (S.StartsWith('select', True) and not ((Length(LastLine) > 0) and (LastLine[LastLine.Length - 1] = '(')))
         or S.StartsWith('execute', True)
         or S.StartsWith('exec', True)
         or S.StartsWith('insert', True)
         or S.StartsWith('update', True)
         or S.StartsWith('delete', True)
         or S.StartsWith('create', True)
         or S.StartsWith('alter', True)
         or S.StartsWith('drop', True)
         or S.StartsWith('truncate', True) then
      begin
        S2 := Sys.ToText(SqlList);
        if not Sys.IsEmpty(S2) then
           SqlStatements.Add(TSqlStatementItem.Create(S2));
        SqlList.Clear();
      end;

      LastLine := S;
      SqlList.Add(TextList[i]);
    end;
    }
  end;

  S2 := Sys.ToText(SqlList);
  if not Sys.IsEmpty(S2) then
     SqlStatements.Add(TSqlStatementItem.Create(S2));

end;


{ TSqlHistory }
constructor TSqlHistory.Create();
begin
  inherited Create();
  FItems := TGenObjectList<TSqlHistoryItem>.Create(True, False);
end;

function TSqlHistory.GetEof: Boolean;
begin
  if FItems.Count = 0 then
     Result := True
  else
    Result := FIndex = FItems.Count - 1;
end;

function TSqlHistory.GetBof: Boolean;
begin
  if FItems.Count = 0 then
    Result := True
  else
    Result := FIndex = 0;
end;

function TSqlHistory.GetCurrent: TSqlHistoryItem;
begin
  Result := nil;
  if (FIndex >= 0) and (FIndex <= FItems.Count - 1) then
    Result := FItems[FIndex];
end;

function TSqlHistory.GetCurrentSqlText: string;
begin
  if Sys.IsEmpty(FCurrentSqlText) then
    Result := ''
  else
    Result := FCurrentSqlText;
end;

procedure TSqlHistory.SetCurrentSqlText(AValue: string);
begin
  if AValue <> FCurrentSqlText then
  begin
    FCurrentSqlText := AValue;
    if Assigned(CurrentSqlTextChanged) then
       CurrentSqlTextChanged(Self);
  end;
end;

procedure TSqlHistory.Next();
begin
  if not (FIndex + 1 > FItems.Count - 1) then
  begin
    Inc(FIndex);
    SetCurrentSqlText(FItems[FIndex].SqlText);
  end else begin
    SetCurrentSqlText('');
  end;
end;

procedure TSqlHistory.Prior();
begin
  if not (FIndex - 1 < 0) then
  begin
    Dec(FIndex);
    SetCurrentSqlText(FItems[FIndex].SqlText);
  end;
end;

procedure TSqlHistory.Add(SqlText: string);
begin
  if not Sys.IsEmpty(SqlText) then
  begin
    if not ((FIndex < 0) or (FIndex > FItems.Count - 1)) then
    begin
      if not Sys.IsSameText(SqlText.Trim(), FItems[FIndex].SqlText.Trim()) then
      begin
        FIndex := FItems.Count;
        FItems.Add(TSqlHistoryItem.Create(SqlText));
      end;
    end else begin
      FIndex := FItems.Count;
      FItems.Add(TSqlHistoryItem.Create(SqlText));
    end;

  end;

end;

end.

