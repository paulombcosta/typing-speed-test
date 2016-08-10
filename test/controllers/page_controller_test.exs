defmodule TypingSpeedTest3.PageControllerTest do
  use TypingSpeedTest3.ConnCase

  test "GET /", %{conn: conn} do
    conn = get conn, "/"
    assert html_response(conn, 200) =~ "Welcome to Phoenix!"
  end
end
