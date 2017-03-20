defmodule Triptych.FileRead do

  def line_to_triple(line) do
    line 
      |> String.rstrip 
      |> String.split(",") 
      |> :erlang.list_to_tuple
  end

  def read(file_name, fun) do
    f = File.open(file_name, [:read])
    case f do
      { :ok, device } -> Enum.map(IO.stream(device, :line), &line_to_triple/1) |> Enum.each(fun)
      { :error, reason } -> reason 
    end  
    #File.close(device)    
  end
  
  def csv_to_store(file_name) do
    read(file_name, &Triptych.Crud.add/1)
  end

end 
