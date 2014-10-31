unit uIOCPCentre;

interface

// call dataReceived procedure with qworker
{.$DEFINE QDAC_QWorker}

uses
  iocpTcpServer, uBuffer, SysUtils, Classes,
  uIocpCoder
  {$IFDEF QDAC_QWorker}
    , qworker
  {$ELSE}
    , iocpTask
  {$ENDIF}
  ;

type
  TIocpCoderSendRequest = class(iocpTcpServer.TIocpSendRequest)
  private
    FBufferLink: TBufferLink;

    FBuf:Pointer;
    FBlockSize: Integer;
  protected
    /// <summary>
    ///   is all buf send completed?
    /// </summary>
    function isCompleted: Boolean; override;

    /// <summary>
    ///  on request successful
    /// </summary>
    procedure onSendRequestSucc; override;

    /// <summary>
    ///   post send a block
    /// </summary>
    function checkSendNextBlock: Boolean; override;


    procedure DoCleanUp;override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure setBufferLink(pvBufferLink:TBufferLink);
  end;


  TIOCPCoderClientContext = class(iocpTcpServer.TIOCPClientContext)
  private
    FrecvBuffers: TBufferLink;
    FStateINfo: String;
    function GetStateINfo: String;
   {$IFDEF QDAC_QWorker}
    procedure OnExecuteJob(pvJob:PQJob);
   {$ELSE}
    procedure OnExecuteJob(pvTaskRequest: TIocpTaskRequest);
   {$ENDIF}
  protected
    procedure add2Buffer(buf:PAnsiChar; len:Cardinal);
    procedure clearRecvedBuffer;
    function decodeObject: TObject;
    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD); override;
    procedure recvBuffer(buf:PAnsiChar; len:Cardinal); virtual;

    procedure DoCleanUp;override;
  protected
  public
    constructor Create;override;
    destructor Destroy; override;

    /// <summary>
    ///   on received a object
    /// </summary>
    /// <param name="pvDataObject"> (TObject) </param>
    procedure dataReceived(const pvDataObject:TObject); virtual;

    /// <summary>
    ///   send a object to peer socket
    /// </summary>
    /// <param name="pvDataObject"> (TObject) </param>
    procedure writeObject(const pvDataObject:TObject);

    /// <summary>
    ///   received buffer
    /// </summary>
    property Buffers: TBufferLink read FrecvBuffers;

    /// <summary>
    ///
    /// </summary>
    property StateINfo: String read GetStateINfo write FStateINfo;
  end;



  TOnDataObjectReceived = procedure(pvClientContext:TIOCPCoderClientContext;pvObject:TObject) of object;

  {$IF RTLVersion>22}
  // thanks: 麦子仲肥19183455
  //  vcl for win64
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  TIOCPConsole = class(TIocpTcpServer)
  private
    FInnerEncoder: TIOCPEncoder;
    FInnerDecoder: TIOCPDecoder;

    FEncoder: TIOCPEncoder;
    FDecoder: TIOCPDecoder;
    FLogicWorkerNeedCoInitialize: Boolean;
    FOnDataObjectReceived: TOnDataObjectReceived;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>
    ///   注册编码器和解码器类
    /// </summary>
    procedure registerCoderClass(pvDecoderClass:TIOCPDecoderClass;
        pvEncoderClass:TIOCPEncoderClass);

    /// <summary>
    ///   register Decoder instance
    /// </summary>
    /// <param name="pvDecoder"> (TIOCPDecoder) </param>
    procedure registerDecoder(pvDecoder:TIOCPDecoder);

    /// <summary>
    ///   register Encoder instance
    /// </summary>
    /// <param name="pvEncoder"> (TIOCPEncoder) </param>
    procedure registerEncoder(pvEncoder:TIOCPEncoder);

  published

    property LogicWorkerNeedCoInitialize: Boolean read FLogicWorkerNeedCoInitialize write FLogicWorkerNeedCoInitialize;
    /// <summary>
    ///   on clientContext received a object
    /// </summary>
    property OnDataObjectReceived: TOnDataObjectReceived read FOnDataObjectReceived
        write FOnDataObjectReceived;




  end;



implementation

uses
  uIOCPFileLogger;

constructor TIOCPCoderClientContext.Create;
begin
  inherited Create;
  FrecvBuffers := TBufferLink.Create();
end;

destructor TIOCPCoderClientContext.Destroy;
begin
  FrecvBuffers.Free;
  inherited Destroy;
end;

procedure TIOCPCoderClientContext.DoCleanUp;
begin
  inherited;
  // clear cache buffer
  FrecvBuffers.clearBuffer;
end;

procedure TIOCPCoderClientContext.add2Buffer(buf: PAnsiChar; len: Cardinal);
begin
  //add to context receivedBuffer
  FrecvBuffers.AddBuffer(buf, len);
end;

procedure TIOCPCoderClientContext.clearRecvedBuffer;
begin
  if FrecvBuffers.validCount = 0 then
  begin
    FrecvBuffers.clearBuffer;
  end else
  begin
    FrecvBuffers.clearHaveReadBuffer;
  end;
end;

procedure TIOCPCoderClientContext.dataReceived(const pvDataObject:TObject);
begin

end;

function TIOCPCoderClientContext.decodeObject: TObject;
begin
  Result := TIocpConsole(Owner).FDecoder.Decode(FrecvBuffers, Self);
end;

function TIOCPCoderClientContext.GetStateINfo: String;
begin
  Result := FStateINfo;
end;



procedure TIOCPCoderClientContext.OnRecvBuffer(buf: Pointer; len: Cardinal;
  ErrCode: WORD);
begin
  recvBuffer(buf, len);
end;

{$IFDEF QDAC_QWorker}
procedure TIOCPCoderClientContext.OnExecuteJob(pvJob: PQJob);
var
  lvObj:TObject;
begin
//  if TIOCPConsole(Owner).FLogicWorkerNeedCoInitialize then
//    pvJob.
    
  lvObj := TObject(pvJob.Data);
  try
    dataReceived(lvObj);
  finally
    lvObj.Free;
  end;
end;
{$ELSE}

procedure TIOCPCoderClientContext.OnExecuteJob(pvTaskRequest: TIocpTaskRequest);
var
  lvObj:TObject;
begin
  try
    if TIOCPConsole(Owner).FLogicWorkerNeedCoInitialize then
      pvTaskRequest.iocpWorker.checkCoInitializeEx();

    lvObj := TObject(pvTaskRequest.TaskData);
    try
      dataReceived(lvObj);
    finally
      lvObj.Free;
    end;
  except
   on E:Exception do
    begin
      TIOCPFileLogger.logErrMessage('截获处理逻辑异常!' + e.Message);
    end;
  end;
end;
{$ENDIF}

procedure TIOCPCoderClientContext.recvBuffer(buf:PAnsiChar; len:Cardinal);
var
  lvObject:TObject;
begin
  add2Buffer(buf, len);

  self.StateINfo := '接收到数据,准备进行解码';

  ////避免一次收到多个包时导致只调用了一次逻辑的处理(dataReceived);
  ///  2013年9月26日 08:57:20
  ///    感谢群内JOE找到bug。
  while True do
  begin
    //调用注册的解码器<进行解码>
    lvObject := decodeObject;
    if Integer(lvObject) = -1 then
    begin
      /// 错误的包格式, 关闭连接
      DoDisconnect;
      exit;
    end else if lvObject <> nil then
    begin
      try
        self.StateINfo := '解码成功,准备调用dataReceived进行逻辑处理';


        if Assigned(TIOCPConsole(Owner).FOnDataObjectReceived) then
          TIOCPConsole(Owner).FOnDataObjectReceived(Self, lvObject);


       {$IFDEF QDAC_QWorker}
         Workers.Post(OnExecuteJob, lvObject);
       {$ELSE}
         iocpTaskManager.PostATask(OnExecuteJob, lvObject);
       {$ENDIF}
      except
        on E:Exception do
        begin
          TIOCPFileLogger.logErrMessage('截获处理逻辑异常!' + e.Message);
        end;
      end;
    end else
    begin
      //缓存中没有可以使用的完整数据包,跳出循环
      Break;
    end;
  end;

  //清理缓存<如果没有可用的内存块>清理
  clearRecvedBuffer;
end;



procedure TIOCPCoderClientContext.writeObject(const pvDataObject:TObject);
var
  lvOutBuffer:TBufferLink;
  lvRequest:TIocpCoderSendRequest;
begin
  if not Active then Exit;
  
  lvOutBuffer := TBufferLink.Create;
  try
    TIocpConsole(Owner).FEncoder.Encode(pvDataObject, lvOutBuffer);
  except
    lvOutBuffer.Free;
    raise;
  end;

  lvRequest := TIocpCoderSendRequest(getSendRequest);
  lvRequest.setBufferLink(lvOutBuffer);


  postSendRequest(lvRequest);

  self.StateINfo := 'TIOCPCoderClientContext.writeObject,投递到发送缓存';

end;

constructor TIOCPConsole.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClientContextClass := TIOCPCoderClientContext;
  FIocpSendRequestClass := TIocpCoderSendRequest;
end;

destructor TIOCPConsole.Destroy;
begin
  if FInnerDecoder <> nil then FInnerDecoder.Free;
  if FInnerEncoder <> nil then FInnerEncoder.Free;
  inherited Destroy;
end;

procedure TIOCPConsole.registerCoderClass(pvDecoderClass: TIOCPDecoderClass;
    pvEncoderClass: TIOCPEncoderClass);
begin
  if FInnerDecoder <> nil then
  begin
    raise Exception.Create('已经注册了解码器类');
  end;

  FInnerDecoder := pvDecoderClass.Create;
  registerDecoder(FInnerDecoder);

  if FInnerEncoder <> nil then
  begin
    raise Exception.Create('已经注册了编码器类');
  end;
  FInnerEncoder := pvEncoderClass.Create;
  registerEncoder(FInnerEncoder);
end;

{ TIOCPConsole }

procedure TIOCPConsole.registerDecoder(pvDecoder: TIOCPDecoder);
begin
  FDecoder := pvDecoder;
end;

procedure TIOCPConsole.registerEncoder(pvEncoder: TIOCPEncoder);
begin
  FEncoder := pvEncoder;
end;

constructor TIocpCoderSendRequest.Create;
begin
  inherited Create;
  FBlockSize := 0;
  FBufferLink := nil;
end;

destructor TIocpCoderSendRequest.Destroy;
begin
  if FBlockSize <> 0 then
  begin
    FreeMem(FBuf);
    FBlockSize := 0;
  end;

  if FBufferLink <> nil then
  begin
    FBufferLink.clearBuffer;
    FBufferLink.Free;
    FBufferLink := nil;
  end;
  inherited Destroy;
end;

procedure TIocpCoderSendRequest.DoCleanUp;
begin
  inherited;

  if FBlockSize <> 0 then
  begin
    FreeMem(FBuf);
    FBlockSize := 0;
  end;

  if FBufferLink <> nil then
  begin
    FBufferLink.clearBuffer;
    FBufferLink.Free;
    FBufferLink := nil;
  end;
end;

{ TIocpCoderSendRequest }

function TIocpCoderSendRequest.checkSendNextBlock: Boolean;
var
  l:Cardinal;
begin
  if FBlockSize = 0 then
  begin
    FBlockSize := Owner.WSASendBufferSize;
    GetMem(FBuf, FBlockSize);
  end;

  l := FBufferLink.readBuffer(FBuf, FBlockSize);
  Result := InnerPostRequest(FBuf, l);
end;

function TIocpCoderSendRequest.isCompleted: Boolean;
begin
  Result := FBufferLink.validCount = 0;

  if Result  then
  begin  // release Buffer
    FBufferLink.clearBuffer;
  end;
end;



procedure TIocpCoderSendRequest.onSendRequestSucc;
begin
  ;
end;

procedure TIocpCoderSendRequest.setBufferLink(pvBufferLink: TBufferLink);
begin
  FBufferLink := pvBufferLink;
end;

end.
