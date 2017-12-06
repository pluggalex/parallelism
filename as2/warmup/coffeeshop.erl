-module(coffeeshop).

-export([run/0, customer/1, barista/0, server/0, cleanup/0]).

run() ->
%	cleanup(),
	io:format("Wake up and smell the coffee!\n"),
	register(barista, spawn(coffeeshop, barista, [])),
	register(server, spawn(coffeeshop, server, [])),
	[ spawn(coffeeshop, customer, [I]) || I <- lists:seq(1, 7) ],
	ok.


customer(Drink) ->
	io:format("Customer wants drink ~p\n", [Drink]),
	Server = whereis(server),
	Server ! {order, Drink, self()},
	receive
		{price, Amount} -> Server ! {pay, self(), Amount * 1.1},
		  customer_wait();
		{sorry} -> notok
	end.

customer_wait() ->
	receive
		{ready, Drink} -> 
			io:format("Customer receives drink ~p\n", [Drink])
	end.

server() ->
	Barista = whereis(barista),
	server(Barista).
	
server(Barista) ->
	receive
		{order, Drink, Customer} ->
			io:format("Server receives order of drink ~p\n", [Drink]),
			Customer ! {price, 10.30},
			receive
				{pay, Customer, Amount} ->
					io:format("Server receives payment $~p\n", [Amount]),
					Barista ! {create, Drink, Customer},
					server(Barista)
			end
	end.

barista() ->
	receive
		{create, Drink, Customer} ->
			io:format("Barista creates drink ~p\n", [Drink]),
			Customer ! { ready, Drink },
			barista()
	end.


cleanup() ->
	unregister(server),
	unregister(barista).
	