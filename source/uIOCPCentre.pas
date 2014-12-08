(*
 *	 Unit owner: D10.Mofen
 *         homePage: http://www.diocp.org
 *	       blog: http://www.cnblogs.com/dksoft
 *
 *   2014-12-06 12:02:18
 *      编码层的0Copy发送数据
          1.压入 TBufferLink队列
          2.检测是否真正发送，如果没有进入开始发送
          3.提取一块进行PostWSASendBuffer。提取失败标志发送标志为False。退出
          4.在发送完成事件中，重复到第三步
 *      结合不得闲意见进行整合修改
 *
 *
 *)
unit uIOCPCentre;

interface

// call dataReceived procedure with qworker
{.$DEFINE QDAC_QWorker}

{$IFDEF DEBUG}
  {$DEFINE DEBUG_ON}
{$ENDIF}

uses
  iocpTcpServer, uBuffer, SysUtils, Classes,
  uIocpCoder, BaseQueue, iocpLocker
  {$IFDEF QDAC_QWorker}
    , qworker
  {$ELSE}
    , iocpTask
  {$ENDIF}
  ;

type
  TIOCPCoderSendRequest = class(TIocpSendRequest)
  private
    FMemBlock:PMemoryBlock;
  protected
    procedure ResponseDone; override;

    procedure CancelRequest;override;

  end;

  TIOCPCoderClientContext = class(iocpTcpServer.TIOCPClientContext)
  private
    ///  正在发送的BufferLink
    FCurrentSendBufferLink: TBufferLink;

    // 待发送队列<TBufferLink队列>
    FSendingQueue: TSimpleQueue;

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
    /// <summary>
    ///   从发送队列中取出一个要发送的对象进行发送
    /// </summary>
    procedure CheckStartPostSendBufferLink;

    /// <summary>
    ///   投递完成后，继续投递下一个请求,
    ///     只在HandleResponse中调用
    /// </summary>
    procedure PostNextSendRequest; override;
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
  FSendingQueue := TSimpleQueue.Create();
  FrecvBuffers := TBufferLink.Create();
end;

destructor TIOCPCoderClientContext.Destroy;
begin
  if IsDebugMode then
  begin
    Assert(FSendingQueue.size = 0);
  end;

  FSendingQueue.Free;
  FrecvBuffers.Free;
  inherited Destroy;
end;

procedure TIOCPCoderClientContext.DoCleanUp;
begin
  /// 清理当前发送队列
  if FCurrentSendBufferLink <> nil then
  begin
    FCurrentSendBufferLink.Free;
  end;

  // 清理释放待发送队列的BufferLink实例 
  FSendingQueue.FreeDataObject;                    

  // 清理已经接收缓存数据
  FrecvBuffers.clearBuffer;
  inherited;
end;

procedure TIOCPCoderClientContext.add2Buffer(buf: PAnsiChar; len: Cardinal);
begin
  //add to context receivedBuffer
  FrecvBuffers.AddBuffer(buf, len);
end;

procedure TIOCPCoderClientContext.CheckStartPostSendBufferLink;
var
  lvMemBlock:PMemoryBlock;
  lvValidCount, lvDataLen: Integer;
  lvSendRequest:TIOCPCoderSendRequest;
begin
  lvDataLen := 0;
  lock();
  try
    // 如果当前发送Buffer为nil 则退出
    if FCurrentSendBufferLink = nil then Exit;

    // 获取第一块
    lvMemBlock := FCurrentSendBufferLink.FirstBlock;

    lvValidCount := FCurrentSendBufferLink.validCount;
    if (lvValidCount = 0) or (lvMemBlock = nil) then
    begin
      // 释放当前发送数据对象
      FCurrentSendBufferLink.Free;
            
      // 如果当前块 没有任何数据, 则获取下一个要发送的BufferLink
      FCurrentSendBufferLink := TBufferLink(FSendingQueue.Pop);
      // 如果当前发送Buffer为nil 则退出
      if FCurrentSendBufferLink = nil then Exit;

      // 获取需要发送的一块数据
      lvMemBlock := FCurrentSendBufferLink.FirstBlock;
      
      lvValidCount := FCurrentSendBufferLink.validCount;
      if (lvValidCount = 0) or (lvMemBlock = nil) then
      begin  // 没有需要发送的数据了
        FCurrentSendBufferLink := nil;  // 没有数据了, 下次压入时执行释放
        exit;      
      end; 
    end;
    if lvValidCount > lvMemBlock.DataLen then
    begin
      lvDataLen := lvMemBlock.DataLen;
    end else
    begin
      lvDataLen := lvValidCount;
    end;


  finally
    unLock();
  end;

  if lvDataLen > 0 then
  begin
    // 从当前BufferLink中移除内存块
    FCurrentSendBufferLink.RemoveBlock(lvMemBlock);

    lvSendRequest := TIOCPCoderSendRequest(GetSendRequest);
    lvSendRequest.FMemBlock := lvMemBlock;
    lvSendRequest.SetBuffer(lvMemBlock.Memory, lvDataLen, dtNone);
    if InnerPostSendRequestAndCheckStart(lvSendRequest) then
    begin
      // 投递成功 内存块的释放在HandleResponse中
    end else
    begin
      lvSendRequest.UnBindingSendBuffer;
      lvSendRequest.FMemBlock := nil;
      lvSendRequest.CancelRequest;

      /// 释放掉内存块
      FreeMemBlock(lvMemBlock);
      
      TIOCPConsole(FOwner).ReleaseSendRequest(lvSendRequest);
    end;
  end;          
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

procedure TIOCPCoderClientContext.PostNextSendRequest;
begin
  inherited;
  CheckStartPostSendBufferLink;
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
  lvStart:Boolean;
begin
  lvStart := false;
  if not Active then Exit;

  if self.LockContext('writeObject', Self) then
  try
    lvOutBuffer := TBufferLink.Create;
    try
      TIocpConsole(Owner).FEncoder.Encode(pvDataObject, lvOutBuffer);
      lock();
      try
        if FSendingQueue.size >= TIocpConsole(Owner).MaxSendingQueueSize then
        begin
          raise Exception.Create('Out of MaxSendingQueueSize!!!');
        end;
        FSendingQueue.Push(lvOutBuffer);
        if FCurrentSendBufferLink = nil then
        begin
          FCurrentSendBufferLink := TBufferLink(FSendingQueue.Pop);
          lvStart := true;
        end;
      finally
        unLock;
      end;
    except
      lvOutBuffer.Free;
      raise;           
    end;
    
    if lvStart then
    begin
      CheckStartPostSendBufferLink;    
    end;
  finally
    self.unLockContext('writeObject', Self);
  end;    
end;

constructor TIOCPConsole.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClientContextClass := TIOCPCoderClientContext;
  
  FIocpSendRequestClass := TIOCPCoderSendRequest;
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



{ TIOCPCoderSendRequest }

procedure TIOCPCoderSendRequest.CancelRequest;
begin
  if FMemBlock <> nil then
  begin
    FreeMemBlock(FMemBlock);
    FMemBlock := nil;
  end;
  inherited;  
end;

procedure TIOCPCoderSendRequest.ResponseDone;
begin
  if FMemBlock <> nil then
  begin
    FreeMemBlock(FMemBlock);
    FMemBlock := nil;
  end;
  inherited;
end;

end.
