(*
 *	 Unit owner: D10.Mofen, delphi iocp framework author
 *         homePage: http://www.Diocp.org
 *	       blog: http://www.cnblogs.com/dksoft
 *
 *    Http协议处理单元
 *    其中大部分思路来自于delphi iocp framework中的iocp.HttpServer
 *
*)
unit DiocpHttpObject;

interface

uses
  Classes, StrUtils, SysUtils, uBuffer,
  iocpTcpServer;

type
  TDiocpHttpState = (hsCompleted, hsRequest{接收请求}, hsRecvingPost{接收数据});
  TDiocpHttpResponse = class;
  TDiocpHttpClientContext = class;
  TDiocpHttpRequest = class(TObject)
  private
    FDiocpContext:TDiocpHttpClientContext;

    /// 头信息
    FHttpVersion: Word;  // 10, 11

    FRequestMethod: string;
    FRequestUrl: String;
    FRequestParams: String;

    FContextType  : string;
    FContextLength: Int64;
    FKeepAlive    : Boolean;
    FRequestAccept: String;
    FRequestReferer:String;
    FRequestAcceptLanguage:string;
    FRequestAcceptEncoding:string;
    FRequestUserAgent:string;
    FRequestAuth:string;
    FRequestCookies:string;
    FRequestHostName:string;
    FRequestHostPort:string;
    FXForwardedFor:string;



    FRawHttpData: TMemoryStream;

    FRawPostData : TMemoryStream;
    FPostDataLen :Integer;

    FRequestHeader: TStringList;

    FResponse: TDiocpHttpResponse;


    /// <summary>
    ///   是否有效的Http 请求方法
    /// </summary>
    /// <returns>
    ///   0: 数据不足够进行解码
    ///   1: 有效的数据头
    ///   2: 无效的请求数据头
    /// </returns>
    function DecodeHttpRequestMethod: Integer;

    /// <summary>
    ///   解码Http请求参数信息
    /// </summary>
    /// <returns>
    ///   1: 有效的Http参数数据
    /// </returns>
    function DecodeHttpRequestParams: Integer;

    /// <summary>
    ///   接收到的Buffer,写入数据
    /// </summary>
    procedure WriteRawBuffer(const Buffer: Pointer; len: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   清理
    /// </summary>
    procedure Clear;

    /// <summary>
    ///  Http响应对象，回写数据
    /// </summary>
    property Response: TDiocpHttpResponse read FResponse;
  end;

  TDiocpHttpResponse = class(TObject)
  private
    FData: TMemoryStream;    
  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   Http 客户端连接
  /// </summary>
  TDiocpHttpClientContext = class(TIocpClientContext)
  private
    FHttpState: TDiocpHttpState;
    FRequest: TDiocpHttpRequest;
  public
    constructor Create; override;
    destructor Destroy; override;
  protected
    /// <summary>
    ///   归还到对象池，进行清理工作
    /// </summary>
    procedure DoCleanUp; override;

    /// <summary>
    ///   接收到客户端的Http协议数据, 进行解码成TDiocpHttpRequest，响应Http请求
    /// </summary>
    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD); override;
  end;

  /// <summary>
  ///  响应请求
  /// </summary>
  TOnDiocpHttpRequest = procedure(pvRequest:TDiocpHttpRequest) of object;

  /// <summary>
  ///   Http 解析服务
  /// </summary>
  TDiocpHttpServer = class(TIocpTcpServer)
  private
    FOnDiocpHttpRequest: TOnDiocpHttpRequest;

    /// <summary>
    ///   响应Http请求， 执行响应事件
    /// </summary>
    procedure DoRequest(pvRequest:TDiocpHttpRequest);
  public
    constructor Create(AOwner: TComponent); override;

    /// <summary>
    ///   响应Http请求事件
    /// </summary>
    property OnDiocpHttpRequest: TOnDiocpHttpRequest read FOnDiocpHttpRequest write
        FOnDiocpHttpRequest;
  end;

implementation

//delphi 最快速编码 URLDecode URLEncode

function URLDecode(const S: string): string;
var
  Idx: Integer;   // loops thru chars in string
  Hex: string;    // string of hex characters
  Code: Integer; // hex character code (-1 on error)
begin
  // Intialise result and string index
  Result := '';
  Idx := 1;
  // Loop thru string decoding each character
  while Idx <= Length(S) do
  begin
    case S[Idx] of
      '%':
      begin
        // % should be followed by two hex digits - exception otherwise
        if Idx <= Length(S) - 2 then
        begin
          // there are sufficient digits - try to decode hex digits
          Hex := S[Idx+1] + S[Idx+2];
          Code := SysUtils.StrToIntDef('$' + Hex, -1);
          Inc(Idx, 2);
        end
        else
          // insufficient digits - error
          Code := -1;
        // check for error and raise exception if found
        if Code = -1 then
          raise SysUtils.EConvertError.Create(
            'Invalid hex digit in URL'
          );
        // decoded OK - add character to result
        Result := Result + Chr(Code);
      end;
      '+':
        // + is decoded as a space
        Result := Result + ' '
      else
        // All other characters pass thru unchanged
        Result := Result + S[Idx];
    end;
    Inc(Idx);
  end;
end;


function URLEncode(const S: string; const InQueryString: Boolean): string;
var
  Idx: Integer; // loops thru characters in string
begin
  Result := '';
  for Idx := 1 to Length(S) do
  begin
    case S[Idx] of
      'A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.':
        Result := Result + S[Idx];
      ' ':
        if InQueryString then
          Result := Result + '+'
        else
          Result := Result + '%20';
      else
        Result := Result + '%' + SysUtils.IntToHex(Ord(S[Idx]), 2);
    end;
  end;
end;

procedure TDiocpHttpRequest.Clear;
begin
  FRawHttpData.Clear;
  FRawPostData.Clear;
  FContextLength := 0;
  FPostDataLen := 0;
end;

constructor TDiocpHttpRequest.Create;
begin
  inherited Create;
  FRawHttpData := TMemoryStream.Create();
  FRawPostData := TMemoryStream.Create();
  FRequestHeader := TStringList.Create();
  FResponse := TDiocpHttpResponse.Create();
end;

destructor TDiocpHttpRequest.Destroy;
begin
  FreeAndNil(FResponse);
  FRawPostData.Free;
  FRawHttpData.Free;
  FRequestHeader.Free;
  inherited Destroy;
end;

function TDiocpHttpRequest.DecodeHttpRequestMethod: Integer;
var
  lvBuf:Pointer;
begin
  Result := 0;
  if FRawHttpData.Size <= 7 then Exit;

  lvBuf := FRawHttpData.Memory;

  if FRequestMethod <> '' then
  begin
    Result := 1;  // 已经解码
    Exit;
  end;

  //请求方法（所有方法全为大写）有多种，各个方法的解释如下：
  //GET     请求获取Request-URI所标识的资源
  //POST    在Request-URI所标识的资源后附加新的数据
  //HEAD    请求获取由Request-URI所标识的资源的响应消息报头
  //PUT     请求服务器存储一个资源，并用Request-URI作为其标识
  //DELETE  请求服务器删除Request-URI所标识的资源
  //TRACE   请求服务器回送收到的请求信息，主要用于测试或诊断
  //CONNECT 保留将来使用
  //OPTIONS 请求查询服务器的性能，或者查询与资源相关的选项和需求
  //应用举例：
  //GET方法：在浏览器的地址栏中输入网址的方式访问网页时，浏览器采用GET方法向服务器获取资源，eg:GET /form.html HTTP/1.1 (CRLF)
  //
  //POST方法要求被请求服务器接受附在请求后面的数据，常用于提交表单。

  Result := 1;
  // HTTP 1.1 支持8种请求
  if (StrLIComp(lvBuf, 'GET', 3) = 0) then
  begin
    FRequestMethod := 'GET';
  end else if (StrLIComp(lvBuf, 'POST', 4) = 0) then
  begin
    FRequestMethod := 'POST';
  end else if (StrLIComp(lvBuf, 'PUT', 3) = 0) then
  begin
    FRequestMethod := 'PUT';
  end else if (StrLIComp(lvBuf, 'HEAD', 3) = 0) then
  begin
    FRequestMethod := 'HEAD';
  end else if (StrLIComp(lvBuf, 'OPTIONS', 7) = 0) then
  begin
    FRequestMethod := 'OPTIONS';
  end else if (StrLIComp(lvBuf, 'DELETE', 6) = 0) then
  begin
    FRequestMethod := 'DELETE';
  end else if (StrLIComp(lvBuf, 'TRACE', 5) = 0) then
  begin
    FRequestMethod := 'TRACE';
  end else if (StrLIComp(lvBuf, 'CONNECT', 7) = 0) then
  begin
    FRequestMethod := 'CONNECT';
  end else
  begin
    Result := 2;
  end;
end;

function TDiocpHttpRequest.DecodeHttpRequestParams: Integer;
var
  lvRawString: AnsiString;
  lvRequestCmdLine, lvMethod, lvTempStr, lvRawTemp:String;
  i, j:Integer;

begin
  Result := 1;
  SetLength(lvRawString, FRawHttpdata.Size);
  FRawHttpdata.Position := 0;
  FRawHttpData.Read(lvRawString[1], FRawHttpdata.Size);
  FRequestHeader.Text := lvRawString;

  // GET /test?v=abc HTTP/1.1
  lvRequestCmdLine := FRequestHeader[0];
  FRequestHeader.Delete(0);

  I := 1;
  while (I <= Length(lvRequestCmdLine)) and (lvRequestCmdLine[I] <> ' ') do
    Inc(I);
  // 请求方法(GET, POST, PUT, HEAD...)
  lvMethod := UpperCase(Copy(lvRequestCmdLine, 1, I - 1));
  Inc(I);
  while (I <= Length(lvRequestCmdLine)) and (lvRequestCmdLine[I] = ' ') do
    Inc(I);
  J := I;
  while (I <= Length(lvRequestCmdLine)) and (lvRequestCmdLine[I] <> ' ') do
    Inc(I);

  // 请求参数及路径
  lvTempStr := Copy(lvRequestCmdLine, J, I - J);
  // 解析参数
  J := Pos('?', lvTempStr);

  if (J <= 0) then
  begin
    FRequestUrl := lvTempStr;
    lvRawTemp := '';

    FRequestUrl := URLDecode(FRequestUrl);
    FRequestParams := '';
  end else
  begin
    FRequestUrl := Copy(lvTempStr, 1, J - 1);
    lvRawTemp := Copy(lvTempStr, J + 1, MaxInt);

    FRequestUrl := URLDecode(FRequestUrl);
    FRequestParams := URLDecode(lvRawTemp);
  end;


  Inc(I);
  while (I <= Length(lvRequestCmdLine)) and (lvRequestCmdLine[I] = ' ') do
    Inc(I);
  J := I;
  while (I <= Length(lvRequestCmdLine)) and (lvRequestCmdLine[I] <> ' ') do
    Inc(I);

  // 请求的HTTP版本
  lvTempStr := Trim(UpperCase(Copy(lvRequestCmdLine, J, I - J)));

  if (lvTempStr = '') then
    lvTempStr := 'HTTP/1.0';
  if (lvTempStr = 'HTTP/1.0') then
  begin
    FHttpVersion := 10;
    FKeepAlive := false;  // 默认为false
  end else
  begin
    FHttpVersion := 11;    
    FKeepAlive := true;    // 默认为true
  end;                     

  FContextLength := 0;


  //eg：POST /reg.jsp HTTP/ (CRLF)
  //Accept:image/gif,image/x-xbit,... (CRLF)
  //...
  //HOST:www.guet.edu.cn (CRLF)
  //Content-Length:22 (CRLF)
  //Connection:Keep-Alive (CRLF)
  //Cache-Control:no-cache (CRLF)
  //(CRLF)         //该CRLF表示消息报头已经结束，在此之前为消息报头
  //user=jeffrey&pwd=1234  //此行以下为提交的数据
  //
  //HEAD方法与GET方法几乎是一样的，对于HEAD请求的回应部分来说，它的HTTP头部中包含的信息与通过GET请求所得到的信息是相同的。利用这个方法，不必传输整个资源内容，就可以得到Request-URI所标识的资源的信息。该方法常用于测试超链接的有效性，是否可以访问，以及最近是否更新。
  //2、请求报头后述
  //3、请求正文(略)

  for i := 0 to FRequestHeader.Count -1 do
  begin
    lvRequestCmdLine := FRequestHeader[i];
    if (lvRequestCmdLine = '') then Continue;

    // 空格之后的第一个字符位置
    j := Pos(' ', lvRequestCmdLine) + 1;

    if StrLIComp(@lvRequestCmdLine[1], 'Content-Type:', 13) = 0 then
      FContextType := Copy(lvRequestCmdLine, j, Length(lvRequestCmdLine))
    else if StrLIComp(@lvRequestCmdLine[1], 'Content-Length:', 15) = 0 then
    begin
      FContextLength := StrToInt64Def(Copy(lvRequestCmdLine, j, MaxInt), -1);
    end
    else if StrLIComp(@lvRequestCmdLine[1], 'Accept:', 7) = 0 then
      FRequestAccept:= Copy(lvRequestCmdLine, j, MaxInt)
    else if StrLIComp(@lvRequestCmdLine[1], 'Referer:', 8) = 0 then
      FRequestReferer := Copy(lvRequestCmdLine, j, MaxInt)
    else if StrLIComp(@lvRequestCmdLine[1], 'Accept-Language:', 16) = 0 then
      FRequestAcceptLanguage := Copy(lvRequestCmdLine, j, MaxInt)
    else if StrLIComp(@lvRequestCmdLine[1], 'Accept-Encoding:', 16) = 0 then
      FRequestAcceptEncoding := Copy(lvRequestCmdLine, j, MaxInt)
    else if StrLIComp(@lvRequestCmdLine[1], 'User-Agent:', 11) = 0 then
      FRequestUserAgent := Copy(lvRequestCmdLine, j, MaxInt)
    else if StrLIComp(@lvRequestCmdLine[1], 'Authorization:', 14) = 0 then
      FRequestAuth := Copy(lvRequestCmdLine, j, MaxInt)
    else if StrLIComp(@lvRequestCmdLine[1], 'Cookie:', 7) = 0 then
      FRequestCookies := Copy(lvRequestCmdLine, j, MaxInt)
    else if StrLIComp(@lvRequestCmdLine[1], 'Host:', 5) = 0 then
    begin
      lvTempStr := Copy(lvRequestCmdLine, j, MaxInt);
      J := Pos(':', lvTempStr);
      if J > 0 then
      begin
        FRequestHostName := Copy(lvTempStr, 1, J - 1);
        FRequestHostPort := Copy(lvTempStr, J + 1, 100);
      end else
      begin
        FRequestHostName := lvTempStr;
        FRequestHostPort := IntToStr((FDiocpContext).Owner.Port);
      end;
    end
    else if StrLIComp(@lvRequestCmdLine[1], 'Connection:', 11) = 0 then
    begin
      lvTempStr := Copy(lvRequestCmdLine, j, MaxInt);
      // HTTP/1.0 默认KeepAlive=False，只有显示指定了Connection: keep-alive才认为KeepAlive=True
      // HTTP/1.1 默认KeepAlive=True，只有显示指定了Connection: close才认为KeepAlive=False
      if FHttpVersion = 10 then
        FKeepAlive := SameText(lvTempStr, 'keep-alive')
      else if SameText(lvTempStr, 'close') then
        FKeepAlive := False;
    end
    else if StrLIComp(@lvRequestCmdLine[1], 'X-Forwarded-For:', 16) = 0 then
      FXForwardedFor := Copy(lvRequestCmdLine, j, MaxInt);
  end;
end;

procedure TDiocpHttpRequest.WriteRawBuffer(const Buffer: Pointer; len: Integer);
begin
  FRawHttpData.WriteBuffer(Buffer^, len);
end;

constructor TDiocpHttpResponse.Create;
begin
  inherited Create;
  FData := TMemoryStream.Create();
end;

destructor TDiocpHttpResponse.Destroy;
begin
  FreeAndNil(FData);
  inherited Destroy;
end;

constructor TDiocpHttpClientContext.Create;
begin
  inherited Create;
  FRequest := TDiocpHttpRequest.Create();
  FRequest.FDiocpContext := Self;
end;

destructor TDiocpHttpClientContext.Destroy;
begin
  FreeAndNil(FRequest);
  inherited Destroy;
end;

procedure TDiocpHttpClientContext.DoCleanUp;
begin
  inherited;
  FHttpState := hsCompleted;
end;

procedure TDiocpHttpClientContext.OnRecvBuffer(buf: Pointer; len: Cardinal;
    ErrCode: WORD);
var
  lvTmpBuf: PAnsiChar;
  CR, LF: Integer;
  lvRemain:Cardinal;
begin
  inherited;
  lvTmpBuf := buf;
  CR := 0;
  LF := 0;
  lvRemain := len;
  while (lvRemain > 0) do
  begin
    if FHttpState = hsCompleted then
    begin  // 完成后重置，重新处理下一个包
      FRequest.Clear;
      FHttpState := hsRequest;
    end;

    if (FHttpState = hsRequest) then
    begin
      case lvTmpBuf^ of
        #13: Inc(CR);
        #10: Inc(LF);
      else
        CR := 0;
        LF := 0;
      end;

      // 写入请求数据
      FRequest.WriteRawBuffer(lvTmpBuf, 1);

      if FRequest.DecodeHttpRequestMethod = 2 then
      begin    // 无效的Http请求
        self.RequestDisconnect('无效的Http请求', Self);
        Exit;
      end;

      // 请求数据已接收完毕(#13#10#13#10是HTTP请求结束的标志)
      if (CR = 2) and (LF = 2) then
      begin
        if FRequest.DecodeHttpRequestParams = 0 then
        begin
          Self.RequestDisconnect('无效的Http协议数据', Self);
          Exit;
        end;


        // 改变Http状态, 进入接受数据状态
        FHttpState := hsRecvingPost;
      end;
    end else if (FHttpState = hsRecvingPost) then
    begin
      Inc(FRequest.FPostDataLen);
      FRequest.FRawPostData.Write(buf^, 1);
      if FRequest.FPostDataLen >= FRequest.FContextLength then
      begin
        FHttpState := hsCompleted;
        // 触发事件
        TDiocpHttpServer(FOwner).DoRequest(FRequest);
      end;
    end;
    Dec(lvRemain);
    Inc(lvTmpBuf);
  end; 
end;


{ TDiocpHttpServer }

constructor TDiocpHttpServer.Create(AOwner: TComponent);
begin
  inherited;
  registerContextClass(TDiocpHttpClientContext);
end;

procedure TDiocpHttpServer.DoRequest(pvRequest: TDiocpHttpRequest);
begin
   if Assigned(FOnDiocpHttpRequest) then
   begin
     FOnDiocpHttpRequest(pvRequest);
   end;
end;

end.
