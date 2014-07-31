unit iocpLocker;

interface

uses
  Windows, SysUtils, Classes;

type
  TIocpLocker = class(TObject)
  private
    FEnterINfo: string;
    FName: String;
    FSection: TRTLCriticalSection;
    function GetEnterCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure lock;
    procedure unLock;


    property EnterCount: Integer read GetEnterCount;
    property EnterINfo: string read FEnterINfo write FEnterINfo;
    property Name: String read FName write FName;

  end;

implementation

constructor TIocpLocker.Create;
begin
  inherited Create;
  InitializeCriticalSection(FSection);
end;

destructor TIocpLocker.Destroy;
begin
  DeleteCriticalSection(FSection);
  inherited Destroy;
end;

function TIocpLocker.GetEnterCount: Integer;
begin
  Result := FSection.RecursionCount;
end;

procedure TIocpLocker.lock;
begin
  if EnterCount > 0 then
  begin
    Name := Name;
  end;
  OutputDebugString(PChar(Format('%d:%s start lock', [GetCurrentThreadID, Name])));
  EnterCriticalSection(FSection);
  OutputDebugString(PChar(Format('%d:%s locked', [GetCurrentThreadID, Name])));
end;

procedure TIocpLocker.unLock;
begin
  LeaveCriticalSection(FSection);
  OutputDebugString(PChar(Format('%d:%s unlock', [GetCurrentThreadID, Name])));
  FEnterINfo := '';
end;

end.
