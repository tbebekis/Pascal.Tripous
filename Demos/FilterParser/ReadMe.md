# Expression Parser

The `TFilterParser` is an expression parser in the `tripous.filterparser.pas`. It can be used in filtering `TDataset`s, hence the name.

The `TFilterParser` is made by following the instructions of the excellent [Crafting Interpreters](https://craftinginterpreters.com/) site. Many many thanks to the author of the site, **Robert Nystrom**. The scanner and parser owes its existence to his work.

The `TFilterParser` is made so that it can be used from any `TDataset` descendant and not only the `TMemTable`. Actually it can be used by any code and not only datasets.

The `TFilterParser` is able to evaluate numerical and boolean expressions and return a result as a `Variant`.

## Supported operators
The `TFilterParser` supports the following operators.

```
<> = > >= < <= - + * / and or not like in
```

## Usage
The user may write an expression such as the following.

```
Name like '%Jo%` and Amount > 5000 
```

The `Name` and `Amount` in the above expression are considered as `Variable`s. The client code has to provide the `Value` of such variables by linking to an event.

```
procedure TMainForm.OnFilterVariableValue(Sender: TObject; Variable: string; ClientTag: Pointer; var Value: Variant);
begin
  if SameText(Variable, 'Name') then
     Value := 'John'
  else if SameText(Variable, 'Amount') then
     Value := 2000
   ;
end;
```

## Example
Here is a simple usage example

```
  Parser := TFilterParser.Create();
  try
    Parser.OnVariable := @OnFilterVariableValue;

    Source := edtFilter.Text;
    Parser.Parse(Source);
 
    V := Parser.Evaluate();
	Result := VarToStr(V);
  finally
    Parser.Free;
  end; 
```

The `TFilterParser.Evaluate()` returns a `Variant`.
 
## Tested On
- FPC 3.2.2
- Lazarus 3.0















