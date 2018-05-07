from pengines.Builder import PengineBuilder
from pengines.Pengine import Pengine
query = "wd ?? continent(X)"
pengine_builder = PengineBuilder(urlserver="http://localhost:9083", destroy=False, ask=query)
pengine = Pengine(builder=pengine_builder, debug=True)
#pengine.create()
#query = "member(X, [1,2,3])"
#pengine.ask(query)
#pengine.doAsk(pengine.currentQuery)
#print()
print(pengine.currentQuery.availProofs, "Has More? ", pengine.currentQuery.hasMore)
#print()
# Get next query.
#print(pengine.state.current_state)
while pengine.currentQuery.hasMore:
    pengine.doNext(pengine.currentQuery)
print(pengine.currentQuery.availProofs)

