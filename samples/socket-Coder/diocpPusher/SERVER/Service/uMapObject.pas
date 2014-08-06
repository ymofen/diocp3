unit uMapObject;
////
///  find()函数 初始化Result
///    2014年7月30日 09:37:33
//// 添加Delete函数
///    2014年1月20日 14:07:30
//// 添加remove函数
////   2014年1月17日 17:21:26
///


interface

uses
  Classes, SysUtils, Windows;

type
  TKeyStr = string[255];
  TEntryBlock = record
    key: TKeyStr;
    data: Pointer;
  end;

  PEntryBlock = ^TEntryBlock;
  
  TMapObject = class(TObject)
  private
    FList: TList;
    function findBlock(const key: TKeyStr): PEntryBlock;
    function findIndex(const key:String): Integer;
    function Getcount: Integer;
    function GetValues(Index: Integer): Pointer;
    function getKeys(Index: Integer): TKeyStr;
  public
    procedure clear;
    constructor Create;
    destructor Destroy; override;

    function exists(const key:string): Boolean;

    function find(const key:string): Pointer;

    procedure remove(const key:string);

    procedure Delete(pvIndex:Integer);

    procedure put(const key: string; const pvData: Pointer);

    property count: Integer read Getcount;
    
    property Values[Index: Integer]: Pointer read GetValues; default;

    property Keys[Index: Integer]: TKeyStr read getKeys;


          


  end;

implementation

procedure TMapObject.clear;
var
  lvBlock:PEntryBlock;
begin
  while FList.Count > 0 do
  begin
    lvBlock := PEntryBlock(FList[0]);
    try
      lvBlock.data := nil;
    except
      //屏蔽掉释放错误
    end;
    FreeMem(lvBlock, SizeOf(TEntryBlock));
    FList.Delete(0);
  end;
end;

constructor TMapObject.Create;
begin
  inherited Create;
  FList := TList.Create();
end;

procedure TMapObject.Delete(pvIndex: Integer);
var
  lvBlock:PEntryBlock;
begin
  if (pvIndex < 0) or (pvIndex >= FList.Count) then
    raise Exception.CreateFmt('keyInterface out of bound[%d]', [pvIndex]);
    
  lvBlock := PEntryBlock(FList[pvIndex]);
  try
    lvBlock.data := nil;
  except
  end;
  FreeMem(lvBlock, SizeOf(TEntryBlock));
  FList.Delete(pvIndex); 
end;

destructor TMapObject.Destroy;
begin
  clear;
  FList.Free;
  inherited Destroy;
end;

function TMapObject.exists(const key:string): Boolean;
begin
  Result := findBlock(TKeyStr(key)) <> nil;
end;

function TMapObject.find(const key:string): Pointer;
var
  lvBlock:PEntryBlock;
begin
  Result := nil;
  lvBlock := findBlock(TKeyStr(key));
  if lvBlock <> nil then
  begin
    Result := lvBlock.data;
  end;
end;

function TMapObject.findBlock(const key: TKeyStr): PEntryBlock;
var
  i:Integer;
  lvBlock:PEntryBlock;
begin
  Result := nil;
  for i := 0 to FList.Count - 1 do
  begin
    lvBlock := PEntryBlock(FList[i]);

    if sameText(string(lvBlock.key), String(key)) then
    begin
      Result := lvBlock;
      Break;
    end;
  end;
end;

function TMapObject.findIndex(const key: String): Integer;
var
  i:Integer;
  lvBlock:PEntryBlock;
begin
  Result := -1;
  for i := 0 to FList.Count - 1 do
  begin
    lvBlock := PEntryBlock(FList[i]);
    if sameText(string(lvBlock.key), key) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TMapObject.Getcount: Integer;
begin
  Result := FList.Count;
end;

function TMapObject.getKeys(Index: Integer): TKeyStr;
var
  lvBlock:PEntryBlock;
begin
  Result := '';
  lvBlock := PEntryBlock(FList[Index]);
  if lvBlock <> nil then
  begin
    Result := lvBlock.key;
  end;                   
end;

function TMapObject.GetValues(Index: Integer): Pointer;
var
  lvBlock:PEntryBlock;
begin
  Result := nil;
  lvBlock := PEntryBlock(FList[Index]);
  if lvBlock <> nil then
  begin
    Result := lvBlock.data;
  end;
end;

procedure TMapObject.put(const key: string; const pvData: Pointer);
var
  lvBlock:PEntryBlock;
  lvkey:TKeyStr;
begin
  lvkey := TKeyStr(key);
  
  lvBlock := findBlock(lvkey);
  if lvBlock <> nil then
  begin
    lvBlock.data := pvData;
  end else
  begin
    GetMem(lvBlock, SizeOf(TEntryBlock));
    ZeroMemory(lvBlock, SizeOf(TEntryBlock));
    lvBlock.key := lvkey;
    lvBlock.data := pvData;
    FList.Add(lvBlock);
  end;
end;

procedure TMapObject.remove(const key: string);
var
  lvBlock:PEntryBlock;
  i:Integer;
begin
  i := findIndex(key);
  if i <> -1 then
  begin
    lvBlock := PEntryBlock(FList[i]);
    lvBlock.data := nil;
    FreeMem(lvBlock, SizeOf(TEntryBlock));
    FList.Delete(i);
  end;
end;

end.
