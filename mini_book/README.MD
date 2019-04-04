
A book includes all securities that the institution regularly buys and sells on
the stock market. A bid is the price that an institution is willing to buy and 
an offer is the price that an institution is willing to sell.

+ Mini-Book receives quotes as input.
+ A quote is a 5-tuple string containing: QuoteID/{B|O}/{N|U|D}/Price/Volume
    - QuoteID : alphanumeric string which uniquely identifies a quote.
    - B/O: whether the quote is for Bid or Offer side.
    - N/U/D: whether this is a new quote or an update or delete for a previously received quote. A quote update can update either volume or price or both. A delete for a quote id of “0” will delete the entire book i.e. 0/B/D/0/0 will delete the entire bid book and 0/O/D/0/0 will delete the entire offer book.
    - Price: price associated with this quote.
    - Volume: volume associated with this quote.
+ Mini-Book then sorts the offers and the bids from the quotes that it receives.
+ For offer, the best price is the lowest price and for bid, the best price is 
      the highest price.
+ The best price is the first price that is displayed.
+ If two quotes have the same price, they are then sorted by volume (from high to low).
+ If two quotes have the same price and volume, they are then ranked by time - most 
      recent to least recent.
+ There should be a function for dumping the contents of the book.


Example:
Given the following sequence of quotes as input:

```
Q1/O/N/1.31/1000000
Q2/B/N/1.21/1000000
Q3/B/N/1.22/1000000
Q4/B/N/1.20/1000000
Q5/B/N/1.20/1000000
Q6/O/N/1.32/1000000
Q7/O/N/1.33/200000
Q5/B/U/1.20/500000
Q7/O/U/1.33/100000
Q7/O/D/0/0
```


MiniBook’s contents should be displayed as follows:

```
OFFER
Q1/1.31/1000000
Q6/1.32/1000000
```

```
BID
Q3/1.22/1000000
Q2/1.21/1000000
Q4/1.20/1000000
Q5/1.20/500000
```
