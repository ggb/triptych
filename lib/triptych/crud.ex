
defmodule Crud do 

  use GenServer.Behaviour
  
  @process_name :triptych_memo
  
  #
  # low-level 
  #
  def init(init_memo) do
    { :ok, { HashDict.new([]), HashDict.new([]), HashDict.new([]) }}
  end
  
  def handle_cast({ :add, triple }, { sub, pre, obj }) do
    
  end
  
  
  # find triples: there are many, many possible cases...
  def handle_call({ :find, { subject, predicate, object } }, { sub, pre, obj }) do
  
  end
  
  def handle_call({ :find, { subject, predicate, :blank } }, { sub, pre, obj }) do
  
  end
  
  def handle_call({ :find, { subject, :blank, :blank } }, { sub, pre, obj }) do
  
  end
  
  def handle_call({ :find, { :blank, predicate, object } }, { sub, pre, obj }) do
  
  end
  
  def handle_call({ :find, { :blank, :blank, object } }, { sub, pre, obj }) do
  
  end
  
  def handle_call({ :find, { subject, :blank, object } }, { sub, pre, obj }) do
  
  end
  
  def handle_call({ :find, { :blank, predicate, :blank } }, { sub, pre, obj }) do
  
  end
  
  # get all
  def handle_call({ :find, { :blank, :blank, :blank } }, { sub, pre, obj }) do
    HashDict.values(sub) |> Enum.reduce [], fn(ele, acc) -> ele ++ acc end 
  end
  
  #  
  # API  
  #
  def start_link(init_memo) do
    :gen_server.start_link({ :local, @process_name }, __MODULE__, init_memo, [])
  end
  
  def add(triple) do
    :gen_server.cast( @process_name, { :add, triple } )
  end
  
  def find(triple) do
    :gen_server.call( @process_name, { :find, triple } )
  end
  
  # def update do
  # 
  # end
  
  def delete(triple) do
    :gen_server.call( @process_name, { :delete, triple } )
  end
  
  def all() do
    find({ :blank, :blank, :blank }) 
  end


end

