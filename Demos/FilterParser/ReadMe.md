

## Filter Expression Parser
`TMemTable` comes with the `TFilterParser`, a filter expression parser in the `tripous.filterparser.pas`.

The `TFilterParser` is made by following the instructions of the excellent [Crafting Interpreters](https://craftinginterpreters.com/) site. Many many thanks to the author of the site, **Robert Nystrom**. The scanner and parser owes its existence to his work.

The `TFilterParser` is made so that it can be used from any `TDataset` descendant and not only the `TMemTable`. Actually it can be used by any code and not only datasets.

The `TFilterParser` supports the following operators.

```
<> = > >= < <= - + * / and or not like in
```

The user may write a filter expression such as the following.

```
Name like '%John` and Amount > 5000 
```
 

 
## Tested On
- FPC 3.2.2
- Lazarus 3.0















