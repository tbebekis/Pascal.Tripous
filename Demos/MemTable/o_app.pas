unit o_App;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
  , SysUtils
  , Forms
  , Controls
  , Graphics
  , Dialogs
  , DBCtrls
  , DBGrids
  ;

type

  { App }

  App = class
    class procedure AdjustGridColumns(Grid: TDBGrid);
  end;

implementation

{ App }

class procedure App.AdjustGridColumns(Grid: TDBGrid);
var
  i : Integer;
begin
  for i := 0 to Grid.Columns.Count-1 do
    Grid.Columns[i].Width := 100;
end;

end.

