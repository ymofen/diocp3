unit SimpleMsgPack;

interface

uses
  classes, SysUtils;

type
  TMsgPackType = (mptUnknown, mptMap, mptString, mptInteger, mptBoolean, mptFloat, mptBinary);
  TSimpleMsgPack = class(TObject)
  private
    FParent:TSimpleMsgPack;

    FName:String;

    FValue:TBytes;

    FDataType:TMsgPackType;

    FChildren: TList;

    function InnerAdd: TSimpleMsgPack;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor TSimpleMsgPack.Create;
begin
  inherited Create;
  FChildren := TList.Create();
end;

destructor TSimpleMsgPack.Destroy;
begin
  FChildren.Free;
  FChildren := nil;
  inherited Destroy;
end;

function TSimpleMsgPack.InnerAdd: TSimpleMsgPack;
begin
  Result := TSimpleMsgPack.Create;
  Result.FParent := self;
  Result.FDataType := mptUnknown;
  FChildren.Add(Result);
end;

end.
