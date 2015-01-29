{ ******************************************************* }
{ }
{ u_class_http_packet }
{ Create By： Locet 2014/8/28 }
{ }
{ ******************************************************* }

unit u_class_http_packet;

interface

uses
  classes, uIOCPCentre, System.SyncObjs, System.SysUtils, System.StrUtils;

type
  TClientState = (csRequest, csPostData, csDone);

  THttpRequest = class
  private
    procedure Reset;
  public
    strs_header_: TStringList;

    data_: RawByteString;
    post_data_: RawByteString;

    request_cmdLine_: string;
    request_content_type_: string;
    request_has_content_length_: Boolean;
    request_content_length_: Int64;
    request_accept_: string;
    request_referer_: string;
    request_accept_language_: string;
    request_accept_encoding_: string;
    request_user_agent_: string;
    request_auth_: string;
    request_cookies_: string;
    request_host_: string;
    request_host_name_: string;
    request_host_port_: string;
    request_connection_: string;
    fx_forwarded_for_: string;

    method_, path_, params_, path_and_params_, version_: string;
    raw_path_, raw_params_, raw_path_and_params_: string;

    http_ver_num_: Integer;
    keep_alive_: Boolean;

    is_accept_post_data_: Boolean;
    post_data_size_: Integer;

    procedure Write(const buffer; const len: Integer);
    function Read(): RawByteString;

    procedure WritePostData(const buffer; const len: Integer);

    function Size(): Integer;
    procedure Clear();
    function IsValidHttpRequest: Boolean;

    constructor Create();
    destructor Destroy; override;
  end;

  THttpResponse = class
  public
    status_: string;
    cont_type_, header_: string;
    cont_size: Integer;
    data_: Pointer;

    procedure Write(const buf; const Size: Integer);

    destructor Destroy; override;

  end;

  THttpClientContext = class(TIOCPCoderClientContext)
  private
    lock_: TSpinLock;
    procedure DataResponse(var Status, ContType, Header: string;
      var ContSize: Integer);
  protected
    procedure DoCleanUp; override;
  public
    http_request_: THttpRequest;
    client_state_: TClientState;
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset();
    function CreateHttpPacket(): THttpRequest;
    function GetData(): THttpRequest;
  protected
    /// <summary>
    /// 数据处理
    /// </summary>
    /// <param name="pvObject"> (TObject) </param>
    procedure dataReceived(const pvObject: TObject); override;
  public
    procedure SendResponse(const Status, ContType, Header: string;
      const ContentSize: Integer; const buf: Pointer);
  end;

function ParseRequestData(const http_request: THttpRequest): Boolean;

implementation

function HexToByte(P: PChar): Byte;
var
  I, B, N: Byte;
begin
  Result := 0;
  for I := 0 to 1 do
  begin
    B := Byte(P[I]);
    case B of
      Byte('0') .. Byte('9'):
        N := B - Byte('0');
    else
      N := (B and $0F) + 9;
    end;
    Result := Result shl 4 + N;
  end;
end;

procedure ByteToHex(B: Byte; P: PChar);
const
  HexChar: array [0 .. 15] of Char = ('0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
begin
  P[0] := HexChar[(B shr 4)];
  P[1] := HexChar[(B and $0F)];
end;

function IsSafeChar(Ch: AnsiChar): Boolean;
const
  // "<>%\^[]`+$,@:;/!#?=&
  UNSAFE_CHARS = ['"', '<', '>', '%', '\', '^', '[', ']', '`', '+', '$', ',',
    '@', ':', ';', '/', '!', '#', '?', '=', '&'];
begin
  Result := not(Ch in UNSAFE_CHARS) and (Byte(Ch) in [33 .. 122]);
end;

function URLEncode(const S: string; Encodeing: TEncoding = nil): string;
var
  I, J: Integer;
  B: Byte;
  RStr: string;
  LEncodeing: TEncoding;
  LBytes: TBytes;
begin
  if (S = '') then
    Exit('');

  if Assigned(Encodeing) then
    LEncodeing := Encodeing
  else
    LEncodeing := TEncoding.UTF8;

  LBytes := LEncodeing.GetBytes(S);
  SetLength(RStr, Length(LBytes) * 3);
  J := 0;
  for I := Low(LBytes) to High(LBytes) do
  begin
    B := LBytes[I];

    if IsSafeChar(AnsiChar(B)) then
    begin
      Inc(J);
      RStr[J] := Char(B);
    end
    else if (AnsiChar(B) = ' ') then
    begin
      Inc(J);
      RStr[J] := '+';
    end
    else
    begin
      Inc(J);
      RStr[J] := '%';
      ByteToHex(B, @RStr[J + 1]);
      Inc(J, 2);
    end;
  end;
  SetLength(RStr, J);

  Result := RStr;
end;

function URLDecode(const S: string; Encodeing: TEncoding = nil): string;
var
  I, J, L: Integer;
  LEncodeing: TEncoding;
  LBytes: TBytes;
  B: Byte;
begin
  if (S = '') then
    Exit('');

  if Assigned(Encodeing) then
    LEncodeing := Encodeing
  else
    LEncodeing := TEncoding.UTF8;

  L := Length(S);
  SetLength(LBytes, L);
  I := 1;
  J := 0;
  while (I <= L) do
  begin
    B := Byte(S[I]);
    if (B = Byte('%')) then
    begin
      B := HexToByte(@S[I + 1]);
      Inc(I, 2);
    end
    else if (B = Byte('+')) then
      B := Byte(' ');
    Inc(I);
    LBytes[J] := B;
    Inc(J);
  end;

  Result := LEncodeing.GetString(LBytes, 0, J);
end;

function ParseRequestData(const http_request: THttpRequest): Boolean;
var
  RequestLine: string;
  I, J, SpacePos: Integer;
begin
  with http_request do
  begin
    strs_header_.Text := data_;
    if (strs_header_.Count = 0) then
      Exit(False);

    // GET /test?v=abc HTTP/1.1
    request_cmdLine_ := strs_header_[0];
    strs_header_.Delete(0);

    I := 1;
    while (I <= Length(request_cmdLine_)) and (request_cmdLine_[I] <> ' ') do
      Inc(I);
    // 请求方法(GET, POST, PUT, HEAD...)
    method_ := UpperCase(Copy(request_cmdLine_, 1, I - 1));
    Inc(I);
    while (I <= Length(request_cmdLine_)) and (request_cmdLine_[I] = ' ') do
      Inc(I);
    J := I;
    while (I <= Length(request_cmdLine_)) and (request_cmdLine_[I] <> ' ') do
      Inc(I);
    // 请求参数及路径

    raw_path_and_params_ := Copy(request_cmdLine_, J, I - J);
    // 解析参数
    J := Pos('?', raw_path_and_params_);
    if (J <= 0) then
    begin
      raw_path_ := raw_path_and_params_;
      raw_params_ := '';

      path_ := URLDecode(raw_path_);
      params_ := '';
      path_and_params_ := path_;
    end
    else
    begin
      raw_path_ := Copy(raw_path_and_params_, 1, J - 1);
      raw_params_ := Copy(raw_path_and_params_, J + 1, MaxInt);

      path_ := URLDecode(raw_path_);
      params_ := URLDecode(raw_params_);
      path_and_params_ := path_ + '?' + params_;
    end;

    Inc(I);
    while (I <= Length(request_cmdLine_)) and (request_cmdLine_[I] = ' ') do
      Inc(I);
    J := I;
    while (I <= Length(request_cmdLine_)) and (request_cmdLine_[I] <> ' ') do
      Inc(I);
    // 请求的HTTP版本
    version_ := Trim(UpperCase(Copy(request_cmdLine_, J, I - J)));
    if (version_ = '') then
      version_ := 'HTTP/1.0';
    if (version_ = 'HTTP/1.0') then
      http_ver_num_ := 10
    else
      http_ver_num_ := 11;
    keep_alive_ := (http_ver_num_ = 11);

    request_has_content_length_ := False;
    request_content_length_ := 0;
    for RequestLine in strs_header_ do
    begin
      if (RequestLine = '') then
        Continue;

      SpacePos := Pos(' ', RequestLine) + 1;

      if StrLIComp(@RequestLine[1], 'Content-Type:', 13) = 0 then
        request_content_type_ := Copy(RequestLine, SpacePos,
          Length(RequestLine))
      else if StrLIComp(@RequestLine[1], 'Content-Length:', 15) = 0 then
      begin
        request_has_content_length_ := TRUE;
        request_content_length_ :=
          StrToInt64Def(Copy(RequestLine, SpacePos, MaxInt), -1);
      end
      else if StrLIComp(@RequestLine[1], 'Accept:', 7) = 0 then
        request_accept_ := Copy(RequestLine, SpacePos, MaxInt)
      else if StrLIComp(@RequestLine[1], 'Referer:', 8) = 0 then
        request_referer_ := Copy(RequestLine, SpacePos, MaxInt)
      else if StrLIComp(@RequestLine[1], 'Accept-Language:', 16) = 0 then
        request_accept_language_ := Copy(RequestLine, SpacePos, MaxInt)
      else if StrLIComp(@RequestLine[1], 'Accept-Encoding:', 16) = 0 then
        request_accept_encoding_ := Copy(RequestLine, SpacePos, MaxInt)
      else if StrLIComp(@RequestLine[1], 'User-Agent:', 11) = 0 then
        request_user_agent_ := Copy(RequestLine, SpacePos, MaxInt)
      else if StrLIComp(@RequestLine[1], 'Authorization:', 14) = 0 then
        request_auth_ := Copy(RequestLine, SpacePos, MaxInt)
      else if StrLIComp(@RequestLine[1], 'Cookie:', 7) = 0 then
        request_cookies_ := Copy(RequestLine, SpacePos, MaxInt)
      else if StrLIComp(@RequestLine[1], 'Host:', 5) = 0 then
      begin
        request_host_ := Copy(RequestLine, SpacePos, MaxInt);
        J := Pos(':', request_host_);
        if J > 0 then
        begin
          request_host_name_ := Copy(request_host_, 1, J - 1);
          request_host_port_ := Copy(request_host_, J + 1, 100);
        end
        else
        begin
          request_host_name_ := request_host_;
          // request_host_port_ := ;
        end;
      end
      else if StrLIComp(@RequestLine[1], 'Connection:', 11) = 0 then
      begin
        request_connection_ := Copy(RequestLine, SpacePos, MaxInt);
        // HTTP/1.0 默认KeepAlive=False，只有显示指定了Connection: keep-alive才认为KeepAlive=True
        // HTTP/1.1 默认KeepAlive=True，只有显示指定了Connection: close才认为KeepAlive=False
        if http_ver_num_ = 10 then
          keep_alive_ := SameText(request_connection_, 'keep-alive')
        else if SameText(request_connection_, 'close') then
          keep_alive_ := False;
      end
      else if StrLIComp(@RequestLine[1], 'X-Forwarded-For:', 16) = 0 then
        fx_forwarded_for_ := Copy(RequestLine, SpacePos, MaxInt);
    end;

    Result := TRUE;
  end;
end;

{ THttpRequest }

constructor THttpClientContext.Create;
begin
  inherited;
  Reset;
end;

function THttpClientContext.CreateHttpPacket: THttpRequest;
begin
  lock_.Enter;
  if http_request_ = nil then
    http_request_ := THttpRequest.Create;
  Result := http_request_;
  lock_.Exit();
end;

procedure THttpClientContext.dataReceived(const pvObject: TObject);
begin
  inherited;
end;

procedure THttpClientContext.DataResponse(var Status, ContType, Header: string;
  var ContSize: Integer);
begin

end;

destructor THttpClientContext.Destroy;
begin
  lock_.Enter;
  if http_request_ <> nil then
    FreeAndNil(http_request_);
  lock_.Exit();

  // data_.Free;
  inherited;
end;

procedure THttpClientContext.DoCleanUp;
begin
  lock_.Enter;
  if http_request_ <> nil then
    FreeAndNil(http_request_);
  lock_.Exit();
  inherited;
end;

function THttpClientContext.GetData: THttpRequest;
begin
  lock_.Enter;
  Result := http_request_;
  http_request_ := nil;
  lock_.Exit();
end;

procedure THttpClientContext.Reset;
begin
  lock_.Enter;
  client_state_ := csRequest;
  if http_request_ <> nil then
    FreeAndNil(http_request_);
  lock_.Exit();
end;

procedure THttpClientContext.SendResponse(const Status, ContType,
  Header: string; const ContentSize: Integer; const buf: Pointer);
var
  response: THttpResponse;
begin
  response := THttpResponse.Create;

  response.status_ := Status;
  response.cont_type_ := ContType;
  response.header_ := Header;
  response.cont_size := ContentSize;
  if ContentSize > 0 then
    response.Write(buf^, ContentSize);

  writeObject(response);
  response.free;
end;

procedure THttpRequest.Reset;
begin
  // FRawRequestText.Size := 0;
  // FRequestPostData.Size := 0;
  // client_state_ := csRequest;
  // FResponseSize := 0;
  // FResponseSent := 0;

  method_ := '';
  path_ := '';
  params_ := '';
  path_and_params_ := '';
  version_ := '';
  http_ver_num_ := 0;
  keep_alive_ := False;

  request_cmdLine_ := '';
  strs_header_.Clear;
  request_content_type_ := '';
  request_has_content_length_ := False;
  request_content_length_ := 0;
  request_accept_ := '';;
  request_referer_ := '';
  request_accept_language_ := '';
  request_accept_encoding_ := '';
  request_user_agent_ := '';
  request_auth_ := '';
  request_cookies_ := '';
  request_host_ := '';
  request_host_name_ := '';
  request_host_port_ := '';
  request_connection_ := '';
  fx_forwarded_for_ := '';

  is_accept_post_data_ := False;
  post_data_size_ := 0;
end;

procedure THttpRequest.Clear;
begin
  data_ := '';
  post_data_ := '';
end;

constructor THttpRequest.Create;
begin
  strs_header_ := TStringList.Create;
  Reset;
end;

destructor THttpRequest.Destroy;
begin
  strs_header_.free;
  inherited;
end;

function THttpRequest.IsValidHttpRequest(): Boolean;
var
  P: PAnsiChar;
  len: Integer;
begin
  len := Size();
  P := PAnsiChar(data_);
  // HTTP 1.1 支持8种请求
  Result := (len > 7) and ((StrLIComp(P, 'GET', 3) = 0) or
    (StrLIComp(P, 'POST', 4) = 0) or (StrLIComp(P, 'PUT', 3) = 0) or
    (StrLIComp(P, 'HEAD', 4) = 0) or (StrLIComp(P, 'OPTIONS', 7) = 0) or
    (StrLIComp(P, 'DELETE', 6) = 0) or (StrLIComp(P, 'TRACE', 5) = 0) or
    (StrLIComp(P, 'CONNECT', 7) = 0));
end;

function THttpRequest.Read(): RawByteString;
begin
  Result := data_;
end;

function THttpRequest.Size: Integer;
begin
  Result := Length(data_);
end;

procedure THttpRequest.Write(const buffer; const len: Integer);
var
  i_len: Integer;
begin
  i_len := Length(data_);
  SetLength(data_, i_len + len);
  Move(buffer, data_[1 + i_len], len);
end;

procedure THttpRequest.WritePostData(const buffer; const len: Integer);
var
  i_len: Integer;
begin
  i_len := Length(post_data_);
  SetLength(post_data_, i_len + len);
  Move(buffer, post_data_[1 + i_len], len);

end;

{ THttpResponse }

destructor THttpResponse.Destroy;
begin
  if data_ <> nil then
    FreeMem(data_);
  inherited;
end;

procedure THttpResponse.Write(const buf; const Size: Integer);
begin
  GetMem(data_, Size);
  Move(buf, data_^, Size);
end;

end.
