unit DoublyLinked;

interface

uses
  SyncObjs, SysUtils;

type
  EIndexOutOfBoundsException = class(Exception);

  TBaseDoublyObject = class(TObject)
  private
    FPre: TBaseDoublyObject;
    FNext:TBaseDoublyObject;

  public
    destructor Destroy; override;
  end;

  /// <example>
  ///
  /// </example>
  TDoublyLinked = class(TObject)
  private
    FSize:Cardinal;
    FFirst: TBaseDoublyObject;
    FLast: TBaseDoublyObject;
    procedure clear;


    procedure linkObj(pvPre, obj, pvNext: TBaseDoublyObject);
    procedure linkToFirst(obj:TBaseDoublyObject);
    procedure linkToLast(obj:TBaseDoublyObject);
    function unLink(obj:TBaseDoublyObject):Boolean;

    function getObjectByPosition(pvIndex:Cardinal): TBaseDoublyObject;
    function postionIsValid(pvIndex:Cardinal): Boolean;
    function outOfBoundMessage(pvIndex:Cardinal): String;

    procedure checkPositionIndex(pvIndex:Cardinal);
  public
    function PeekFirst: TBaseDoublyObject;
    function PeekLast: TBaseDoublyObject;

    procedure addToFirst(obj:TBaseDoublyObject);
    procedure addToLast(obj:TBaseDoublyObject);

    procedure remove(obj:TBaseDoublyObject);

    function isLinked(obj:TBaseDoublyObject):Boolean;

    function indexOf(obj:TBaseDoublyObject):Integer;

    function calcuSize:Cardinal;

    property Size: Cardinal read FSize;
  end;

  /// <example>
  ///  use in thread
  /// </example>
  TSafeDoublyLinkedList = class(TObject)
  private
    FLinkedList: TDoublyLinked;
    FLocker: TCriticalSection;
    function GetSize: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function PeekFirst: TBaseDoublyObject;
    function PeekLast: TBaseDoublyObject;

    procedure addToFirst(obj:TBaseDoublyObject);
    procedure addToLast(obj:TBaseDoublyObject);

    procedure remove(obj:TBaseDoublyObject);

    function checkRemove(obj:TBaseDoublyObject):Boolean;

    function containsObject(obj:TBaseDoublyObject):Boolean;

    function isLinked(obj:TBaseDoublyObject):boolean;

    function calcuSize():Cardinal;

    property Size: Integer read GetSize;
  end;

implementation

procedure TDoublyLinked.addToFirst(obj: TBaseDoublyObject);
begin
  linkToFirst(obj);
end;

procedure TDoublyLinked.addToLast(obj:TBaseDoublyObject);
begin
  linkToLast(obj);
end;

function TDoublyLinked.calcuSize: Cardinal;
var
  lvObj:TBaseDoublyObject;
begin
  lvObj := FFirst;
  Result := 0;
  while lvObj <> nil do
  begin
    lvObj := lvObj.FNext;
    Inc(Result);
  end;
end;

procedure TDoublyLinked.checkPositionIndex(pvIndex: Cardinal);
begin
  if (not postionIsValid(pvIndex)) then
     raise EIndexOutOfBoundsException.Create(outOfBoundMessage(pvIndex));
end;

procedure TDoublyLinked.clear;
begin
  FFirst := nil;
  FLast := nil;
  FSize := 0;
end;

function TDoublyLinked.getObjectByPosition(pvIndex:Cardinal): TBaseDoublyObject;
var
  i:Cardinal;
  x:TBaseDoublyObject;
begin
  if pvIndex <(FSize shr 1) then
  begin
    x := FFirst;
    for i := 0 to pvIndex -1 do
    begin
      x := X.FNext;
    end;
    Result := x;
  end else
  begin
    x := FLast;
    for i := FSize -1 downto pvIndex - 1  do
    begin
      x := X.FPre;
    end;
    Result := x;
  end;
end;

function TDoublyLinked.indexOf(obj: TBaseDoublyObject): Integer;
var
  i : Integer;
  lvObj:TBaseDoublyObject;
begin
  Result := -1;
  if obj <> nil then
  begin
    lvObj := FFirst;
    i := 0;
    while lvObj <> nil do
    begin
      if lvObj = obj then
      begin
        Result := i;
        exit;
      end;
      lvObj := lvObj.FNext;
      inc(i);
    end;
  end;
end;

function TDoublyLinked.isLinked(obj: TBaseDoublyObject): Boolean;
begin
  Result := (obj.FPre <> nil) or (obj.FNext <> nil);
end;

procedure TDoublyLinked.linkObj(pvPre, obj, pvNext: TBaseDoublyObject);
begin
  obj.FPre := pvPre;
  obj.FNext := pvNext;

end;

procedure TDoublyLinked.linkToFirst(obj:TBaseDoublyObject);
var
  lvFirst:TBaseDoublyObject;
begin
  lvFirst := FFirst;
  linkObj(nil, obj, lvFirst);
  FFirst := obj;
  if lvFirst = nil then   // link is empty
    FLast := obj
  else
    lvFirst.FPre := obj;
  Inc(FSize);
end;

procedure TDoublyLinked.linkToLast(obj: TBaseDoublyObject);
var
  lvLast:TBaseDoublyObject;
begin
  lvLast := FLast;
  linkObj(lvLast, obj, nil);
  FLast := obj;
  if lvLast = nil then   // link is empty
    FFirst := obj
  else
    lvLast.FNext := obj;
  Inc(FSize);
end;

function TDoublyLinked.outOfBoundMessage(pvIndex:Cardinal): String;
begin
  Result := 'Index: '+ IntToStr(pvIndex)+', Size: '+intToStr(FSize);
end;

function TDoublyLinked.PeekFirst: TBaseDoublyObject;
begin
  Result := FFirst;
end;

function TDoublyLinked.PeekLast: TBaseDoublyObject;
begin
  Result := FLast;
end;

function TDoublyLinked.postionIsValid(pvIndex:Cardinal): Boolean;
begin
  Result := (pvIndex >=0) and (pvIndex <= FSize);
//    private boolean isPositionIndex(int index) {
//        return index >= 0 && index <= size;
//    }
end;

procedure TDoublyLinked.remove(obj: TBaseDoublyObject);
begin
  unLink(obj);
end;

function TDoublyLinked.unLink(obj: TBaseDoublyObject): Boolean;
var
  lvPre, lvNext:TBaseDoublyObject;
begin
  lvPre := obj.FPre;
  lvNext := obj.FNext;

//  if (obj.FPre = nil) and (obj.FNext = nil) then
//  begin
//    Result := false;
//  end;

  // link pre object
  if (lvPre = nil) then
    FFirst := lvNext
  else
  begin
    lvPre.FNext := lvNext;
    obj.FPre := nil;
  end;

  // link next Object
  if (lvNext = nil) then
    FLast := lvPre
  else
  begin
    lvNext.FPre := lvPre;
    obj.FNext := nil;
    Result := true;
  end;

  Dec(FSize);
end;

procedure TSafeDoublyLinkedList.addToFirst(obj: TBaseDoublyObject);
begin
  FLocker.Enter;
  try
    FLinkedList.addToFirst(obj);
  finally
    FLocker.Leave;
  end;
end;

procedure TSafeDoublyLinkedList.addToLast(obj: TBaseDoublyObject);
begin
  FLocker.Enter;
  try
    FLinkedList.addToLast(obj);
  finally
    FLocker.Leave;
  end;
end;

function TSafeDoublyLinkedList.calcuSize: Cardinal;
begin
  FLocker.Enter;
  try
    Result := FLinkedList.calcuSize;
  finally
    FLocker.Leave;
  end;
end;

function TSafeDoublyLinkedList.checkRemove(obj: TBaseDoublyObject): Boolean;
begin
  FLocker.Enter;
  try
    Result := FLinkedList.indexOf(obj) <> -1;
    if Result then
    begin
      FLinkedList.remove(obj);
    end;
  finally
    FLocker.Leave;
  end;
end;

function TSafeDoublyLinkedList.containsObject(obj: TBaseDoublyObject): Boolean;
begin
  FLocker.Enter;
  try
    Result := (FLinkedList.indexOf(obj) <> -1);

    if not Result  then
    begin
      FLinkedList.FSize := FLinkedList.FSize;
    end;

  finally
    FLocker.Leave;
  end;
end;

constructor TSafeDoublyLinkedList.Create;
begin
  inherited Create;
  FLocker := TCriticalSection.Create();
  FLinkedList := TDoublyLinked.Create();
end;

destructor TSafeDoublyLinkedList.Destroy;
begin
  FLocker.Free;
  FLinkedList.Free;
  inherited Destroy;
end;

function TSafeDoublyLinkedList.GetSize: Integer;
begin
  Result := FLinkedList.Size;
end;

function TSafeDoublyLinkedList.isLinked(obj: TBaseDoublyObject): boolean;
begin
  Result := FLinkedList.isLinked(obj);
end;

function TSafeDoublyLinkedList.PeekFirst: TBaseDoublyObject;
begin
  FLocker.Enter;
  try
    Result := FLinkedList.PeekFirst;
  finally
    FLocker.Leave;
  end;
end;

function TSafeDoublyLinkedList.PeekLast: TBaseDoublyObject;
begin
  FLocker.Enter;
  try
    Result := FLinkedList.PeekLast;
  finally
    FLocker.Leave;
  end;
end;

procedure TSafeDoublyLinkedList.remove(obj: TBaseDoublyObject);
begin
  FLocker.Enter;
  try
    FLinkedList.remove(obj);
  finally
    FLocker.Leave;
  end;
end;

destructor TBaseDoublyObject.Destroy;
begin
  FPre := nil;
  FNext := nil;
  inherited Destroy;
end;

{ TDoublyLinked }


end.
