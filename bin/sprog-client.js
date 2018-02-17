pengines = require('pengines');

peng = pengines({
    server: "http://127.0.0.1:9083/pengine",
    //ask: "member(X,[a,b,c,d,7])",
    ask: "t(X)",
    chunk: 100,
    //sourceText: "t(X):- (go ?? rdf(X,rdf:type,owl:'ObjectProperty')).",
    //sourceText: "t(X):- (go ?? rdf(X,rdf:type,owl:'ObjectProperty')).\n",
    sourceText: "t(X):- (wd ?? continent(X)).\n",

}
).on('success', handleSuccess).on('error', handleError);
function handleSuccess(result) {
    console.log('# Results: '+ result.data.length);
    for (var i = 0; i < result.data.length; i++) {
        console.log(result.data[i])
    }
    if (result.data.length == 0) {
        console.log("No results!")
    }
}
function handleError(result) {
    console.error(result)
}
