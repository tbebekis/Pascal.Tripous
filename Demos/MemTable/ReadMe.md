# Crafting an in-memory TDataset descendant in Free Pascal - Lazarus: the ultimate adventure.

This text describes the adventure of creating the [**TMemTable**](https://github.com/tbebekis/Pascal.Tripous/tree/main/Demos/MemTable), a [TDataset](https://www.freepascal.org/docs-html/fcl/db/tdataset.html) descendant in [Free Pascal](https://www.freepascal.org/) and [Lazarus](https://www.lazarus-ide.org/).

The `TMemTable` is an in-memory `TDataset` which has most of the features a Pascal developer expects from a TDataset:

- Bookmarks
- Lookup and Calculated fields
- Master-detail link
- Sorting on multiple fields
- Record Status filtering  
- Range filtering
- Filtering based on an expression
- `Locate()` and `Lookup()` methods
- Blobs
- Load from and Save to XML.

## What is TDataset
`TDataset` is a [Pascal](https://en.wikipedia.org/wiki/Pascal_(programming_language)) class which represents database data as columns and rows. 

`TDataset` uses the notion of [**Active Record pattern**](https://en.wikipedia.org/wiki/Active_record_pattern). A data row, a `record` in TDataset parlance, is considered the **active** one, upon which operations, such as `Insert()`, `Edit()` or `Delete()`, may applied.

`TDataset` is an old construction. It started its existance with [Delphi](https://en.wikipedia.org/wiki/History_of_Delphi_(software)) back in 1995.

Back in those days an application kept open a connection to a database throughout its lifetime. The code that presented database data to the application retrieved data from the database one row at a time or in small groups of rows. `TDataset` adheres to this logic. So the **Active Record pattern** was almost the only way to go, considering the limited memory of those computers.

`TDataset` operations, such as `Insert()`, `Edit()` or `Delete()`, are applied to the **active** `Record Buffer`. A `Record Buffer` is a [Pointer](https://en.wikipedia.org/wiki/Pointer_(computer_programming)) to an array of Bytes. `TDataset` uses a lot of buffers for various purposes. Even a totally empty `TDataset` allocates memory for more than 10 record buffers.

# Inheriting TDataset

Creating a `TDataset` descendant is not an easy task. And, believe it or not, there are a lot fo "secrets" and close to zero help.

`TDataset` contains [abstract methods](https://en.wiktionary.org/wiki/abstract_method) and [virtual methods](https://en.wiktionary.org/wiki/virtual_method) that are just stubs.

A `TDataset` descendant, such as the `TMemTable`, has to override these abstract and virtual methods and provide its own implementation.

Here is the absolute minimum of methods a descendant class has to override in order to provide the basic functionality. They are divided into groups to easy the discussion.

```
  TMemTable = class(TDataSet)
    ...
  protected
    { record buffer }
    function  AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var RecBuf: TRecordBuffer); override;
    function  GetRecord(RecBuf: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function  GetRecordSize: Word; override;

    { bookmark }
    function  GetBookmarkFlag(RecBuf: TRecordBuffer): TBookmarkFlag; override;
    procedure SetBookmarkFlag(RecBuf: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure GetBookmarkData(RecBuf: TRecordBuffer; Data: Pointer); override;
    procedure SetBookmarkData(RecBuf: TRecordBuffer; Data: Pointer); override;
    procedure InternalGotoBookmark(pBM: Pointer); override;
    procedure InternalSetToRecord(RecBuf: TRecordBuffer); override;

    { navigation }
    procedure InternalFirst; override;
    procedure InternalLast; override;

    { editing }
    procedure InternalInitRecord(RecBuf: TRecordBuffer); override;
    procedure InternalPost; override;
    procedure InternalAddRecord(RecBuf: Pointer; IsAppend: Boolean); override;
    procedure InternalEdit; override;
    procedure InternalCancel; override;
    procedure InternalDelete; override;

    { open/close }
    function  IsCursorOpen: Boolean; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
  public
    ...

    { fields }
    function  GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    function  CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;

    { bookmark }
    function  BookmarkValid(BM: TBookmark): Boolean; override;
    function  CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;

    ...

  end;
```

## The Record Buffer

A `Record Buffer` is an array of bytes in [heap memory](https://en.wikipedia.org/wiki/Memory_management#Manual_memory_management). That byte array has a layout dictated by the number of fields and their datatypes. Plus some more information necessary to identify the buffer and its properties.

In the old days it was a custom to provide a diagram of the buffer layout when inheriting `TDataset`. For the `TMemTable` is the following.

```
   +-----------------+-------------------+-------------------+-----------------+
   |                 |                   |                   |                 |
   |   Data Fields   |    Calc Fields    |    Blob fields    |    TRecInfo     |
   |                 |                   |                   |                 |
   +-----------------+-------------------+-------------------+-----------------+
   0                 CalcOfs             BlobOfs             BookOfs
                                                                               
```

The actual data of fields of all kinds are stored directly in the buffer, except of the [Blob](https://en.wikipedia.org/wiki/Object_storage) fields.

A blob in `TMemTable`  is represented by the following Pascal types.

```
    PBlob = ^TBlob;
    TBlob = packed record
        Size    : PtrUInt;
        Data    : Pointer;
    end; 
```

For any blob field a `TBlob` structure is stored in the record buffer only. The actual blob data, where the `Data` field points to, are in the heap memory.

Except of the actual record data a record buffer contains information about the record buffer. In `TMemTable` this information is represented by the `TRecInfo` Pascal record.

```
    PRecInfo = ^TRecInfo;
    TRecInfo = record
        Id             : LongWord;        // unique record Id
        Bookmark       : TIntBM;          // the BookMark
        BookmarkFlag   : TBookmarkFlag;   // = (bfCurrent, bfBOF, bfEOF, bfInserted)
        Status         : TUpdateStatus;   // = (usUnmodified, usModified, usInserted, usDeleted)
    end;   
```

The `TIntBM` type is declared by `TMemTable` code as following.

```
    // The following type is used a bookmark data
    TIntBM = PtrUInt;
    PIntBM = ^TIntBM; 
```

`TDataset` bookmark methods use an untyped Pointer as bookmark parameter, in order to pass or retrieve a bookmark. So there is a need to typecast that pointer to and from an integer.

`TMemTable` uses an integer of type [`PtrUInt`](https://www.freepascal.org/docs-html/rtl/system/ptruint.html) for that typecast, following the documentation which clearly states that: "*PtrUInt is an unsigned integer type which has always the same size as a pointer. When using integers which will be cast to pointers and vice versa, use this type*".

A `TDataset` bookmark is a way to mark a data record and then use that bookmark, at a later time, to go back to the bookmarked record, that is designating the record as the active record.

## Record Buffer methods

One of the first things a `TDataset` descendant has to do is to calculate the size, **in bytes**, of the record buffer. The `TMemTable` performs this calculation in the `Initialize()` method. The result is stored in the `FRecBufSize` protected field.

The actual `FRecBufSize` is the sum of the [`TField.DataSize`](https://www.freepascal.org/docs-html/current/fcl/db/tfield.datasize.html) property. For blob fields that `DataSize` is the `SizeOf(TBlob)`.

The `TRecordBuffer` type represents a record buffer and is defined, by Free Pascal, as following.

```
type
    PChar         = ^Char;    
    PAnsiChar     = PChar; 
    TRecordBuffer = PAnsiChar; 
```

The above tell us that a `TRecordBuffer` is actually a [`PChar`](https://www.freepascal.org/docs-html/ref/refsu12.html), a Pointer to Char.


Here are the record buffer methods.

```
    function  AllocRecordBuffer(): TRecordBuffer; override;
    procedure FreeRecordBuffer(var RecBuf: TRecordBuffer); override;
    function  GetRecord(RecBuf: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function  GetRecordSize(): Word; override;
```

### function  GetRecordSize(): Word; override;
This method is the getter for the `TDataset.RecordSize` property. It should return the size, **in bytes**, of the record buffer.

`TDataset` code does not use it at all. It's probably for use by descendants.

The `TMemTable` returns the `FRecBufSize`, which is the full size of the record buffer, including the `TRecInfo` record. 

### function  AllocRecordBuffer(): TRecordBuffer; override;  
`TDataset` uses alot of record buffers. This method is called by `TDataset`, in order to allocate memory of the proper size, whenever a new buffer is needed.

The code of this method should allocate memory for the new record buffer, calling `AllocMem()`, and then should call `TDataset.InitRecord()` passing the newly allocated record buffer.
 
It may then proceed to other record buffer initialization tasks, specific to the `TDataset` descendant.
 
### procedure FreeRecordBuffer(var RecBuf: TRecordBuffer); override;
This method is called by `TDataset` whenever it needs to free the memory of a record buffer.

The code of this method should set the passed in buffer to `nil`, after freeing the memory.

### function  GetRecord(RecBuf: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;

This method is called by `TDataset` whenever it needs to retrieve a record.

The `TGetMode` is defined as `TGetMode = (gmCurrent, gmNext, gmPrior);`. So the `GetMode` parameter indicates the position of the record the `TDataset` has to retrieve, relative to current record.

The `DoCheck` parameter may passed by `TDataset` as `True` in some cases. 
 
The result of the `GetRecord()` function is of type `TGetResult` which is defined as 
```
TGetResult = (
    grOK,       // everything is ok, a record buffer is retrieved
    grBOF,      // Befin Of File, in the crack before the first record, no record buffer
    grEOF,      // End Of File, in the crack after the last record, no record buffer
    grError     // an error occured, no record buffer, if DoCheck = True then raise an exception
);
```

In case where `DoCheck` is `True` and `grError` is going to be returned, the method should raise an exception.
 

The `RecBuf` parameter is where the function should place the record data if the result is `grOK`.

Depending on the `TDataset` descendant, that record data could be stored in various places, i.e. a database or a file.

The `TMemTable` is an **in-memory** dataset. Its storage medium is a [`TList`](https://www.freepascal.org/docs-html/rtl/classes/tlist.html). The `FCurRecIndex` private field is the index to the current record in that `TList`. So the  `TMemTable.GetRecord()` just copies the record buffer from its internal `TList` to that `RecBuf` parameter.

```
    ...

     if (Result = grOK) and Assigned(RecBuf) then
     begin
       SourceRecBuf := FRows[FCurRecIndex];
       CopyRecord(SourceRecBuf, RecBuf, True);

       RecInfo := GetRecInfo(RecBuf);
       RecInfo^.BookmarkFlag := bfCurrent;
     end else if (Result = grError) and DoCheck then
       DatabaseError('No records'); 

    ...
```

## Bookmark methods

A `TDataset` bookmark is a way to mark a data record and then use that bookmark, at a later time, to go back to the bookmarked record, that is designating the record as the active record.

Here are the bookmark methods.
```
  protected
    ...
    { bookmark }
    function  GetBookmarkFlag(RecBuf: TRecordBuffer): TBookmarkFlag; override;
    procedure SetBookmarkFlag(RecBuf: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure GetBookmarkData(RecBuf: TRecordBuffer; Data: Pointer); override;
    procedure SetBookmarkData(RecBuf: TRecordBuffer; Data: Pointer); override;
    procedure InternalGotoBookmark(pBM: Pointer); override;
    procedure InternalSetToRecord(RecBuf: TRecordBuffer); override;   
    ...
  public
    ...
    { bookmark }
    function  BookmarkValid(BM: TBookmark): Boolean; override;
    function  CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;   
    ...

```

The `TBookmarkFlag` denotes where the bookmarked data record is located, and it is defined as following.

```
type
    TBookmarkFlag = (
        bfCurrent,      // on the current record
        bfBOF,          // on the Beginning Of File
        bfEOF,          // on the End Of File
        bfInserted      // on the last inserted data record
    ); 
```

The `TMemTable` uses the following `TRecInfo` Pascal record to keep bookmark related information. A single `TRecInfo` is appended to the end of the record buffer of each data record.

```
    type
      PRecInfo = ^TRecInfo;
      TRecInfo = record
        Id             : LongWord;        // unique record Id
        Bookmark       : TIntBM;          // the BookMark
        BookmarkFlag   : TBookmarkFlag;   // = (bfCurrent, bfBOF, bfEOF, bfInserted)
        Status         : TUpdateStatus;   // = (usUnmodified, usModified, usInserted, usDeleted)
      end; 
```

Then the `TMemTable` uses a function to get a pointer to that `TRecInfo` type.

```
  function  TMemTable.GetRecInfo(RecBuf: Pointer): PRecInfo;
  var
      P : PByte;
  begin
      P := PByte(RecBuf) + FBookOfs;
      Result := PRecInfo(P);
  end;   
```

As you can see, pointer arithmetic is used in order to move the passed in `RecBuf` pointer to the correct offset, where the `TRecInfo` resides.

### function  GetBookmarkFlag(RecBuf: TRecordBuffer): TBookmarkFlag; override;

This method is called by `TDataset` whenever it needs to retrieve the flag of a bookmark.

### procedure SetBookmarkFlag(RecBuf: TRecordBuffer; Value: TBookmarkFlag); override;

This method is called by `TDataset` whenever a bookmark flag has to be set.

```
procedure TMemTable.SetBookmarkFlag(RecBuf: TRecordBuffer; Value: TBookmarkFlag);
var
  RecInfo: PRecInfo;
begin
  RecInfo := GetRecInfo(RecBuf);
  RecInfo^.BookmarkFlag := Value;
end;  
```

### procedure GetBookmarkData(RecBuf: TRecordBuffer; Data: Pointer); override;

This method is called by `TDataset` whenever it needs to retrieve the actual value of a bookmark.
 
### procedure SetBookmarkData(RecBuf: TRecordBuffer; Data: Pointer); override;

This method is called by `TDataset` whenever a bookmark has to be set.

```
procedure TMemTable.SetBookmarkData(RecBuf: TRecordBuffer; Data: Pointer);
var
  RecInfo : PRecInfo;
  BM      : TIntBM;
begin
  BM := PIntBM(Data)^;

  RecInfo := GetRecInfo(RecBuf);
  RecInfo^.Bookmark := BM;
end;
```

### procedure InternalGotoBookmark(pBM: Pointer); override;

This method is called by `TDataset` whenever it needs to go to the record marked with the passed in parameter and make it the current record.

The passed in parameter is the *bookmark data*, and for the `TMemTable` this is just an integer.

The `TMemTable` has to go to its internal `TList` of record buffers, locate a record marked with the passed in `pBM` value, and set the `FCurRecIndex` accordingly.

```
function TMemTable.IndexOfBookmark(BM: TIntBM): Integer;
var
  i     : Integer;
  RecInfo: PRecInfo;
begin
  Result := -1;
  if BM > 0 then
  begin
    for i := 0 to FRows.Count - 1 do
    begin
      RecInfo := GetRecInfo(FRows[i]);
      if (BM = RecInfo^.Bookmark) then
      begin
        Result := i;
        Break; //==>
      end;
    end;
  end;
end;  

function TMemTable.GoToBookmarkInternal(BM: TIntBM): Boolean;
var
  Index  : Integer;
begin
  Result := False;

  Index := IndexOfBookmark(BM);

  if Index <> -1 then
  begin
    Result := True;
    FCurRecIndex := Index;
  end;
end;   

procedure TMemTable.InternalGotoBookmark(pBM: Pointer);
var
  BM : TIntBM;
begin
  BM := PIntBM(pBM)^;
  GoToBookmarkInternal(BM);
end;  

```

### procedure InternalSetToRecord(RecBuf: TRecordBuffer); override;   

This method is called by `TDataset` whenever it needs to make the data record, passed in with the`TRecordBuffer` parameter, to be the current record.

```
procedure TMemTable.InternalSetToRecord(RecBuf: TRecordBuffer);
var
  RecInfo : PRecInfo;
  BM      : TIntBM;
begin
  RecInfo := GetRecInfo(RecBuf);

  BM := RecInfo^.Bookmark;
  GoToBookmarkInternal(BM);
end;
```
 
### function  BookmarkValid(BM: TBookmark): Boolean; override; 
This method may called by user code to check the validity of a bookmark, previously retrieved using the [`Bookmark`](https://www.freepascal.org/docs-html/fcl/db/tdataset.bookmark.html) property.

### function  CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
This method may called by user code in order to compare two bookmarks, previously retrieved using the [`Bookmark`](https://www.freepascal.org/docs-html/fcl/db/tdataset.bookmark.html) property.

This method should return the following compare result.

```
   1 when A > B
   0 when A = B
  -1 when A < B
```

## Navigation methods

`TDataset` contains a number of public and protected navigation methods, such as `First()`, `Last()` and `MoveBy()`. Navigation methods used in repositioning the current record.

Two protected navigation methods must be overriden. 

```
    procedure InternalFirst; override;
    procedure InternalLast; override;
```

The `TMemTable` just sets the `FCurRecIndex` field to the BOF or EOF crack value accordingly.

### procedure InternalFirst; override;

This method is called by `TDataset` and makes the first record, if any, to be the current record.

```
procedure TMemTable.InternalFirst;
begin
  FCurRecIndex := -1;
end;    
``` 

### procedure InternalLast; override;

This method is called by `TDataset` and makes the last record, if any, to be the current record.

```
procedure TMemTable.InternalLast;
begin
  FCurRecIndex := FRows.Count;
end;  
``` 

## Editing methods

A number of *internal* methods must be overriden in order to have functioning `Insert()`, `Edit()`, `Delete()` etc. methods.

```
    procedure InternalInitRecord(RecBuf: TRecordBuffer); override;
    procedure InternalPost; override;
    procedure InternalAddRecord(RecBuf: Pointer; IsAppend: Boolean); override;
    procedure InternalEdit; override;
    procedure InternalCancel; override;
    procedure InternalDelete; override;
```

### procedure InternalInitRecord(RecBuf: TRecordBuffer); override;

This method is called by the `TDataset.InitRecord()` and `TDataset.ClearFields()` methods to give the descendant dataset a chance to initialize the record buffer, usually by filling it with zeroes.

```
procedure TMemTable.InternalInitRecord(RecBuf: TRecordBuffer);
begin
   FillChar(RecBuf^, FRecBufSize, 0);    
end; 
```

### procedure InternalPost; override;

This method is called by the `TDataset.Post()`. 

Client code has to call `Post()` after a call to `Insert()`, `Append()` or `Edit()`. This means that the `InternalPost()` will be called subsequently after an insert or edit operation.

This method should copy the data of the active record buffer, along with the full bookmark information, to the storage medium the descendant dataset uses.

The `TDataset.ActiveBuffer()` method returns the active record buffer.

### procedure InternalAddRecord(RecBuf: Pointer; IsAppend: Boolean); override;

This method is **not** called by `TDataset` code. 

A `TDataset` descendant should call this method when the `Post()` is called, and a data record is to be **inserted** or **appended**.

When the `IsAppend` parameter is `True` then the data record should be appended as the last data record. Otherwise the data record should be inserted at the current record position.

The `TMemTable` calls this method from inside the `InternalPost()`, when the `Post()` is called after a call to `Insert()` or `Append()`.

### procedure InternalEdit; override;

This method is called by the `TDataset.Edit()`. 

It gives a chance to the dataset descendant to prepare for an edit operation.

### procedure InternalCancel; override;

This method is called by the `TDataset.Cancel()`. 

It gives a chance to the dataset descendant to undo whatever has done after the last `Insert()`, `Append()` or `Edit()` call, which is now being cancelled.

The `TMemTable` clears its internal list of modified fields and frees any allocated blobs.

```
procedure TMemTable.InternalCancel;
begin
  inherited InternalCancel;
  FModifiedFields.Clear;
  FreeBlobs(TRecordBuffer(ActiveBuffer()));
end;
```

### procedure InternalDelete; override;

This method is called by the `TDataset.Delete()`. It should delete the current record from the list of records.

## Open and Close methods

A number of methods related to opening and closing the `TDataset` should be overriden.

```
    function  IsCursorOpen: Boolean; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
```


### function  IsCursorOpen: Boolean; override;

This method is called by the `TDataset` to check if it is open and ready, which could be two different things.

The `TMemTable` uses its own `Initialize()` method which is called from inside of the `InternalOpen()` as a consequence of calling `Open()` or setting the `Active` property to `True`.

The `TMemTable.Initialize()` sets the `FInitialized` private field to `True` upon successful exit.

The `TMemTable.IsCursorOpen()` returns the `FInitialized` value.

```
function TMemTable.IsCursorOpen: Boolean;
begin
  Result := FInitialized;
end; 
```

### procedure InternalOpen; override;

This method is called by the `TDataset.Open()`.

The dataset descendant should initialize any private fields, establish a connection to its storage medium, create the `TField` instances, and *bind* those `TField` instances to actual data.

 
### procedure InternalClose; override;

This method is called by the `TDataset.Close()`.

The dataset descendant should free any object instances, memory or other resources, allocated while the dataset was open.

## Field methods

One of the most crucial operations of a `TDataset` descendant code is reading and writing to **field data buffers**.

Non-blob field data buffers are handled by the `GetFieldData()` and `SetFieldData()` methods.

For blob fields the `GetFieldData()` and `SetFieldData()` methods just read the proper `TBlob` Pascal record, found in the record data buffer, in order to know where the actual blob data is.

Blob fields require special handling which involves a custom [`TStream`](https://www.freepascal.org/docs-html/rtl/classes/tstream.html).

```
    function  GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    function  CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
```

### function  GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;

This method is called by the `TDataset` in two cases:
- when it needs the field data
- when it needs to know if the field is `NULL` or not.

The passed in `Buffer` parameter is **not** `nil` when the `TDataset` wants the field data.

The passed in `Buffer` parameter **is** `nil` when the `TDataset` just wants to know if the field is `NULL` or not.  

The method has first to get the active record buffer.
 
If the passed in `Buffer` parameter is **not** `nil` then the method has to copy the field data from the active record buffer to that `Buffer` parameter, and return `True`. If the field data is `nil`, i.e. `NULL`, then leaves the `Buffer` untouched and just returns `False`.

If the passed in `Buffer` parameter **is** `nil` then the method has just to check if the field data is `nil`, i.e. `NULL`, or not. The method should return `True` if field data is not `nil` and `False` otherwise.

When the `TMemTable` calculates the size, in bytes, of a record data buffer, it adds an extra byte in every non-blob field buffer. Then it uses that byte as the `NULL` flag. For blob fields it uses the `Size` property of the `TBlob` Pascal record. 

If the first byte of a non-blob field buffer is zero or the `Size` property of a `TBlob` field is zero, then the field is considered `NULL`.

```
function TMemTable.IsFieldNull(RecBuf: Pointer; FieldIndex: Integer): Boolean;
var
  P : PByte;
  Blob : PBlob;
begin
  if not IsBlobFieldType(FFieldTypes[FieldIndex]) then
  begin
    P := GetFieldPtr(RecBuf, FieldIndex);
    Result := PByte(P)^ = 0;
  end else begin
    Blob   := GetBlobPtr(RecBuf, FieldIndex);
    Result := Blob^.Size = 0;
  end;
end; 
```
 
### procedure SetFieldData(Field: TField; Buffer: Pointer); override;

This method is called by the `TDataset`.

The method should copy field data from the passed in `Buffer` parameter to the active record data buffer at the position where the field is located.

If the passed in `Buffer` is `nil` then it means that the field should be set to `NULL` in the record data buffer.
 

### function  CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;

This method is called by the `TBlobField` whenever blob data is about to read or written.

A `TDataset` descendant has to provide a `TStream` class in its implementation in order to handle blobs.

The `TMemTable` provides its own `TBlobStream` class.

```
function TMemTable.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
var
  RecBuf: Pointer;
begin
  RecBuf := nil;
  GetActiveRecBuf(RecBuf);
  Result := TBlobStream.Create(Self, TBlobField(Field), RecBuf, Mode);
end;
```

# TMemTable specific

`TMemTable` functionality resides in two Pascal code units

- tripous.memtable.pas
- tripous.filterparser.pas

The first unit `uses` the second one. No other dependencies exist. So `TMemTable` can be used just by adding `tripous.memtable.pas` in a `uses` clause.

## Demo application

There is a tiny demo application in the folder `\Demos\MemTable`. The application displays most of the `TMemTable` functionality and can be used for testing its capabilities.

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

## Load from and save to XML

The "*load from/save to XML*" functionality resides in a totally independent `full static class` which may serve other `TDataset` implementations too, and not only the `TMemTable`.

 ```
  XmlPersistor = class
    class function  CreateXmlDoc(RootName: string; aEncoding: string): TXMLDocument;

    class function  ToXmlDoc(Table: TDataset; SchemaOnly: Boolean = False): TXMLDocument;
    class function  ToXmlText(Table: TDataset; SchemaOnly: Boolean = False): string;
    class procedure SaveToXmlFile(Table: TDataset; const FileName: string; SchemaOnly: Boolean = False);
    class procedure SaveToXmlStream(Table: TDataset; Stream: TStream; SchemaOnly: Boolean = False);

    class procedure FromXmlDoc(Table: TDataset; Doc: TXMLDocument; CreateDatasetProc: TCreateDatasetProc; SchemaOnly: Boolean = False);
    class procedure FromXmlText(Table: TDataset; XmlText: string; CreateDatasetProc: TCreateDatasetProc; SchemaOnly: Boolean = False);
    class procedure LoadFromXmlFile(Table: TDataset; const FileName: string; CreateDatasetProc: TCreateDatasetProc; SchemaOnly: Boolean = False);
    class procedure LoadFromXmlStream(Table: TDataset; Stream: TStream; CreateDatasetProc: TCreateDatasetProc; SchemaOnly: Boolean = False);
  end;
 ```
 
## Tested On
- FPC 3.2.2
- Lazarus 3.0















