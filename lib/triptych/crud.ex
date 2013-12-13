
defmodule Triptych.Crud do 

  use GenServer.Behaviour
  
  @process_name :triptych_memo
  
  #
  # low-level 
  #
  def init(init_memo) do
    { :ok, { HashDict.new(init_memo), HashDict.new(init_memo), HashDict.new(init_memo) }}
  end
  
  def handle_cast({ :add, triple = { subject, predicate, object } }, { sub, pre, obj }) do
    { :noreply, { add_helper(subject, triple, sub), add_helper(predicate, triple, pre), add_helper(object, triple, obj) } }
  end  
  
  # find triples: there are many, many possible cases...
  def handle_call({ :find, triple }, _from, dicts = { sub, pre, obj }) do
    case triple do
      { :blank, :blank, :blank }     ->
        found = HashDict.values(sub) |> Enum.reduce HashSet.new, &HashSet.union/2
      { :blank, :blank, object }     ->
        found = dict_get obj, object
      { :blank, predicate, :blank }  ->
        found = dict_get pre, predicate
      { subject, :blank, :blank }    ->
        found = dict_get sub, subject
      { subject, predicate, :blank }  ->
        found = HashSet.intersection dict_get(sub, subject), dict_get(pre, predicate)
      { :blank, predicate, object }  ->
        found = HashSet.intersection dict_get(obj, object), dict_get(pre, predicate)
      { subject, :blank, object }    ->
        found = HashSet.intersection dict_get(sub, subject), dict_get(obj, object)
      { subject, predicate, object } -> 	
	    found = HashSet.intersection HashSet.new([{ subject, predicate, object }]), dict_get(sub, subject) 
    end
    { :reply, found, dicts }
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
    :gen_server.call( @process_name, { :find, triple } ) |> HashSet.to_list
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

  #
  # Helper
  #
  def add_helper(key, value, dict) do
    if(HashDict.has_key?(dict, key)) do
      # Update list
      HashDict.update! dict, key, fn(set) -> HashSet.union(set, HashSet.new([ value ])) end
    else
      # Create list, add value
      HashDict.put dict, key, HashSet.new([ value ])
    end
  end
  
  def dict_get(dict, key) do
    HashDict.get( dict, key, HashSet.new ) 
  end

end

