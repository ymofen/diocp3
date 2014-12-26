(*
 *	 Unit owner: d10.天地弦
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   2014-12-12 17:00:28
 *     同步与服务端uiocpCentre中的处理方式
 *
 *)


unit iocpCoderClient;


interface

uses
  iocpClientSocket, iocpBaseSocket, uIocpCoder,
  uBuffer, SysUtils, Classes, BaseQueue;

type
  TOnDataObjectReceived = procedure(pvObject:TObject) of object;

  TIOCPCoderSendRequest = class(TIocpSendRequest)
  private
    FMemBlock:PMemoryBlock;
  protected
    procedure ResponseDone; override; 
    procedure CancelRequest;override;
  end;


  TIocpCoderRemoteContext = class(TIocpRemoteContext)
  private
    ///  正在发送的BufferLink
    FCurrentSendBufferLink: TBufferLink;

    // 待发送队列<TBufferLink队列>
    FSendingQueue: TSimpleQueue;

    FRecvBufferLink: TBufferLink;

    FInnerEncoder: TIOCPEncoder;
    FInnerDecoder: TIOCPDecoder;

    FEncoder: TIOCPEncoder;
    FDecoder: TIOCPDecoder;
    FOnDataObjectReceived: TOnDataObjectReceived;
  protected
    /// <summary>
    ///   从发送队列中取出一个要发送的对象进行发送
    /// </summary>
    procedure CheckStartPostSendBufferLink;
    /// <summary>
    ///   on recved data, run in iocp worker thread
    /// </summary>
    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; errCode: WORD); override;

    /// <summary>
    ///   投递完成后，继续投递下一个请求,
    ///     只在HandleResponse中调用
    /// </summary>
    procedure PostNextSendRequest; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    /// <summary>
    ///   注册编码器和解码器类
    /// </summary>
    procedure registerCoderClass(pvDecoderClass:TIOCPDecoderClass;
        pvEncoderClass:TIOCPEncoderClass);
    /// <summary>
    ///   注册解码器
    /// </summary>
    /// <param name="pvDecoder"> (TIOCPDecoder) </param>
    procedure registerDecoder(pvDecoder:TIOCPDecoder);

    /// <summary>
    ///   注册编码器
    /// </summary>
    /// <param name="pvEncoder"> (TIOCPEncoder) </param>
    procedure registerEncoder(pvEncoder:TIOCPEncoder);


    /// <summary>
    ///   发送一个对象到服务端
    /// </summary>
    procedure writeObject(pvObject:TObject);

  public

    /// <summary>
    ///   接收到一个对象
    /// </summary>
    property OnDataObjectReceived: TOnDataObjectReceived read FOnDataObjectReceived write FOnDataObjectReceived;
  end;


  TIocpCoderClient = class(TIocpClientSocket)
  public
    constructor Create(AOwner: TComponent); override;
  end;






implementation

uses
  uIOCPFileLogger;


constructor TIocpCoderRemoteContext.Create;
begin
  inherited Create;
  FRecvBufferLink := TBufferLink.Create();

  FSendingQueue := TSimpleQueue.Create();
end;

destructor TIocpCoderRemoteContext.Destroy;
begin
  if IsDebugMode then
  begin
    Assert(FSendingQueue.size = 0);
  end;  
  FSendingQueue.Free;

  FreeAndNil(FRecvBufferLink);
  if FInnerDecoder <> nil then FInnerDecoder.Free;
  if FInnerEncoder <> nil then FInnerEncoder.Free;
  inherited Destroy;
end;

procedure TIocpCoderRemoteContext.CheckStartPostSendBufferLink;
var
  lvMemBlock:PMemoryBlock;
  lvValidCount, lvDataLen: Integer;
  lvSendRequest:TIOCPCoderSendRequest;
begin
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
      
      TIocpCoderClient(Owner).ReleaseSendRequest(lvSendRequest);
    end;
  end;          
end;

procedure TIocpCoderRemoteContext.OnRecvBuffer(buf: Pointer; len: Cardinal;
    errCode: WORD);
var
  lvObject:TObject;
begin
  //inherited OnRecvBuffer(buf, len, errCode);
  FRecvBufferLink.AddBuffer(buf, len);

  while True do
  begin
    //调用注册的解码器<进行解码>
    lvObject := FDecoder.Decode(FRecvBufferLink, Self);
    if Integer(lvObject) = -1 then
    begin
      self.Close;
      exit;
    end else if lvObject <> nil then
    begin
      try
        try
          if Assigned(FOnDataObjectReceived) then
            FOnDataObjectReceived(lvObject);
        except
          on E:Exception do
          begin
            TIOCPFileLogger.logErrMessage('截获处理逻辑异常!' + e.Message);
          end;
        end;
      finally
        lvObject.Free;
      end;
    end else
    begin
      //缓存中没有可以使用的完整数据包,跳出循环
      Break;
    end;
  end;

  //清理缓存<如果没有可用的内存块>清理
  if FRecvBufferLink.validCount = 0 then
  begin
    FRecvBufferLink.clearBuffer;
  end else
  begin
    FRecvBufferLink.clearHaveReadBuffer;
  end;
end;

procedure TIocpCoderRemoteContext.PostNextSendRequest;
begin
  inherited PostNextSendRequest;
  CheckStartPostSendBufferLink;
end;

procedure TIocpCoderRemoteContext.registerCoderClass(pvDecoderClass: TIOCPDecoderClass;
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

{ TIocpCoderRemoteContext }

procedure TIocpCoderRemoteContext.registerDecoder(pvDecoder: TIOCPDecoder);
begin
  FDecoder := pvDecoder;
end;

procedure TIocpCoderRemoteContext.registerEncoder(pvEncoder: TIOCPEncoder);
begin
  FEncoder := pvEncoder;
end;


procedure TIocpCoderRemoteContext.writeObject(pvObject: TObject);
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
      FEncoder.Encode(pvObject, lvOutBuffer);
      lock();
      try
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

{ TIocpCoderClient }

constructor TIocpCoderClient.Create(AOwner: TComponent);
begin
  inherited;
  registerContextClass(TIocpCoderRemoteContext);
  FIocpSendRequestClass := TIocpCoderSendRequest;
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
