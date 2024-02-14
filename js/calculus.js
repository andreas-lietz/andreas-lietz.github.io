const symbols = new Map([
    ['conjunction', '\\wedge'],
    ['disjunction', '\\vee'],
    ['implication', '\\rightarrow'],
    ['exists', '\\exists'],
    ['forall', '\\forall']
]);

function union(setA, setB) {
    var _union = new Set(setA);
    for (var elem of setB) {
        _union.add(elem);
    }
    return _union;
}
function eqSets(setA, setB){
    if (setA.size != setB.size){
    return false;
    }
    return [...setA].every(x => setB.has(x));
}
function freeVariablesFromSequence(sequence){
    let variables = new Set();
    sequence.forEach(fml => variables = union(variables, fml.freeVariables()));
    return variables;
}

//Terms
class Term{
    constructor(){
    }
    findNextBlank(){
        if (this instanceof Variable || this instanceof BlankTerm){
            return null;
        }
        if (this instanceof CompositeTerm){
            let i;
            let blank = null;
            for (i=0; i < this.arity; i++){
                if (this.subterms[i] instanceof BlankTerm){
                    return {term: this, index: i};
                }
                blank = this.subterms[i].findNextBlank();

                if (blank != null){
                    return blank;
                }
            }
            return null;
        }
    }
}
class Variable extends Term{
    constructor(symbol){
        super();
        this.symbol = symbol;
    }
    latex(){
        return this.symbol;
    }
    freeVariables() {
        return new Set([this.symbol]);
    }

    insertAtBlank(){
        return false;
    }
    isComplete(){
        return true;
    }
    substitute(substituent, substitutor) {
        if(substituent instanceof Variable && areTheSame(this,substitutor)){
                this.symbol = substituent.symbol;
        }
    }
    deepCopy(){
        return new Variable(this.symbol);
    }
}
let infixSymbols = {
    '+' : '+',
    '*' : '\\cdot'
}
class CompositeTerm extends Term{
    constructor(arity, symbol, subterms){
        super();
        this.arity = arity;
        this.symbol = symbol;
        this.subterms = subterms;
    }
    latex(){
        if (this.arity === 0){
            return this.symbol;
        }
        if (this.symbol === '+' || this.symbol === '*'){
            return '(' + this.subterms[0].latex() + ' ' + infixSymbols[this.symbol] + ' ' + this.subterms[1].latex() + ' )'
        }
        let str = this.symbol+'(';
        let i;
        for (i=0; i < this.arity; i++){
            str += this.subterms[i].latex();
            if (i+1 < this.arity){
                str += ',\\ '
            }
        }
        return str + ')'
    }
    freeVariables() {
        let variables = new Set();
        this.subterms.forEach(term => variables = union(variables, term.freeVariables()));
        return variables;
    }

    insertAtBlank(filler){
        if(!(filler instanceof Term)){
            alert('Wrong Syntax!');
            return false;
        }
        for(let i=0; i< this.subterms.length; i++){
            let sub = this.subterms[i];
            if(sub instanceof BlankTerm){
                this.subterms[i] = filler;
                return true;
            }
            if(sub.insertAtBlank(filler)){
                return true;
            }
        }
        return false;
    }
    isComplete(){
        let complete = true;
        this.subterms.forEach(term => complete = complete && term.isComplete());
        return complete;
    }
    substitute(substituent, substitutor) {
        for(let i = 0; i < this.arity; i++){
            let term = this.subterms[i];
            if(areTheSame(term,substitutor)){
                if(!(substituent instanceof Term)){
                    alert('Wrong Syntax! The substituent must be a term.');
                }
                this.subterms[i] = substituent;
            }
            else {
                term.substitute(substituent, substitutor);
            }
        }
    }
    deepCopy(){
        let copiedTerms = [];
        this.subterms.forEach(term => copiedTerms.push(term.deepCopy()));
        return new CompositeTerm(this.arity,this.symbol, copiedTerms);
    }
}
class BlankTerm extends Term{
    constructor(){
        super();
        this.symbol ='\\_';
    }
    freeVariables(){
        return new Set();
    }
    latex(){
        return this.symbol;
    }
    isComplete(){
        return false;
    }
    substitute(varout, varin) {
        return
    }
    deepCopy(){
        return new BlankTerm();
    }
}


//Formulas
class Formula{
    constructor(){
    }
    neg(){
        return  new NegFormula(this);
    }
    implies(fml){
        return new ConcatFormula(this, 'implication', fml);
    }
    disjunct(fml){
        return new ConcatFormula(this, 'disjunction', fml);
    }
    conjunct(fml){
        return new ConcatFormula(this, 'conjunction', fml);
    }
    exists(variable){
        return new QuantifierFormula('exists', variable, this);
    }
    forall(variable){
        return new QuantifierFormula('forall', variable, this);
    }
    quantify(type, variable){
        return new QuantifierFormula(type, variable, this);
    }
    freeVariables(){
        let variables = new Set();
        for (let sub in this.subformulas){
            variables = union(variables, sub.freeVariables())
        }
        return variables;
    }
    addSubstitution(substituent,substitutor){
        return new SubstitutionFormula(this, substituent,substitutor);
    }
}
class AtomicFormula extends Formula{
    constructor(arity, symbol, terms){
        super();
        this.arity = arity;
        this.symbol = symbol;
        this.terms = terms;
    }
    freeVariables(){
        let variables = new Set();
        for (let i=0; i < this.arity; i++){
            variables = union(variables, this.terms[i].freeVariables());
        }
        return variables;
    }
    latex(){
        let symbol = this.symbol;
        if (symbol === '<'){
            symbol = '\\lt';
        }
        if (symbol === '>'){
            symbol = '\\gt';
        }
        if (this.arity === 0){
            return this.symbol;
        }
        if (this.arity === 2 && symbol !== 'P' && symbol !== 'Q') {
            return this.terms[0].latex() + ' ' + symbol + ' ' + this.terms[1].latex();
        } else {
            let str = this.symbol+'(';
            var i;
            for (i = 0; i < this.arity; i++) {
                str += this.terms[i].latex();
                if(i + 1 < this.arity){
                    str += ',\\ '
                }
            }
            return str + ')';
        }
    }
    insertAtBlank(filler){
        for(let i = 0; i< this.arity; i++){
            let subterm = this.terms[i];
            if(subterm instanceof BlankTerm){
                filler instanceof Term ? this.terms[i] = filler : alert('Wrong Syntax! Expecting term!');
                return true;
            }
            if(subterm.insertAtBlank(filler)) {
                return true;
            }
        }
        return false;
    }
    findNextBlank(){
        let i;
        for(i=0; i < this.arity; i++){
            let blank = null;
            if(this.terms[i] instanceof BlankTerm){
                return {formula: this, index: i}
            }
            blank = this.terms[i].findNextBlank();
            if(blank != null){
                return blank;
            }
        }
        return null;
    }
    isComplete(){
        let complete = true;
        this.terms.forEach(term => complete = complete && term.isComplete());
        return complete;
    }
    substitute(substituent, substitutor){
        for(let i = 0; i < this.arity; i++){
            let term = this.terms[i];
            if(areTheSame(term,substitutor)){
                if(!(substituent instanceof Term)){
                    alert('Wrong Syntax! Substituent must be a term.');
                    return;
                }
                this.terms[i] = substituent;
            }
            else{
                term.substitute(substituent,substitutor);
            }
        }
    }
    deepCopy(){
        let copiedTerms = [];
        this.terms.forEach(term => copiedTerms.push(term.deepCopy()));
        return new AtomicFormula(this.arity,this.symbol,copiedTerms);
    }
}
class ConcatFormula extends Formula{
    constructor(fml1, type, fml2) {
        super();
        this.subformulaLeft = fml1;
        this.type = type;
        this.subformulaRight = fml2;
        this.subformulas = [fml1,fml2];
        this.symbol = symbols.get(type)
    }
    latex(){
        return '(' + this.subformulaLeft.latex() + ' ' + this.symbol + ' ' + this.subformulaRight.latex() + ')';
    }
    insertAtBlank(filler){
        if(this.subformulaLeft instanceof BlankFormula){
            filler instanceof Formula ? this.subformulaLeft = filler : alert('Wrong Syntax! Expecting formula!');
            return true;
        }
        if(this.subformulaLeft.insertAtBlank(filler)){
            return true;
        }
        if(this.subformulaRight instanceof BlankFormula){
            filler instanceof Formula ? this.subformulaRight = filler : alert('Wrong Syntax! Expecting formula!');
            return true;
        }
        return this.subformulaRight.insertAtBlank(filler);
    }
    freeVariables() {
        return union(this.subformulaLeft.freeVariables(), this.subformulaRight.freeVariables());
    }

    findNextBlank() {
        if(this.subformulaLeft instanceof BlankFormula){
            return {formula: this, index: 'subformulaLeft'};
        }
        let blank = this.subformulaLeft.findNextBlank();
        if(blank != null){
            return blank;
        }
        if(this.subformulaRight instanceof BlankFormula){
            return {formula: this, index: 'subformulaRight'};
        }
        return this.subformulaRight.findNextBlank();
    }
    isComplete(){
        return this.subformulaLeft.isComplete() && this.subformulaRight.isComplete();
    }
    substitute(substituent, substitutor){
        this.subformulaLeft.substitute(substituent,substitutor);
        this.subformulaRight.substitute(substituent,substitutor);
    }
    deepCopy(){
        return new ConcatFormula(this.subformulaLeft.deepCopy(), this.type, this.subformulaRight.deepCopy());
    }
}
class NegFormula extends Formula{
    constructor(fml){
        super();
        this.subformula = fml;
        this.subformulas = [fml];
        this.symbol = '\\neg'
    }
    latex(){
        return '\\neg ' + this.subformula.latex();
    }
    freeVariables() {
        return this.subformula.freeVariables();
    }
    insertAtBlank(filler){
        if(this.subformula instanceof BlankFormula){
            filler instanceof Formula ? this.subformula = filler : alert('Wrong Syntax! Expecting formula!');
            return true;
        }
        return this.subformula.insertAtBlank(filler);
    }
    isComplete(){
        return this.subformula.isComplete();
    }

    findNextBlank() {
        if(this.subformula instanceof BlankFormula){
            return {formula:this, index: 'subformula'};
        }
        return this.subformula.findNextBlank();
    }
    substitute(substituent,substitutor){
        this.subformula.substitute(substituent,substitutor);
    }
    deepCopy(){
        return new NegFormula(this.subformula.deepCopy());
    }
}
class QuantifierFormula extends Formula{
    constructor(type, variable, fml){
        super();
        this.type = type;
        this.variable = variable;
        this.subformula = fml;
        this.subformulas = [fml];
        this.symbol = symbols.get(type)

    }
    latex(){
        return this.symbol + ' ' + this.variable.symbol + ' (' + this.subformula.latex() + ')';
    }
    freeVariables() {
        let variables = this.subformula.freeVariables();
        variables.delete(this.variable.symbol);
        return variables;
    }

    insertAtBlank(filler){
        if(this.variable instanceof BlankTerm){
            filler instanceof Variable ? this.variable = filler : alert('Wrong Syntax! Expecting variable!');
            return true;
        }
        if(this.subformula instanceof BlankFormula){
            filler instanceof Formula ? this.subformula = filler : alert('Wrong Syntax! Expecting formula!');
            return true;
        }
        return this.subformula.insertAtBlank(filler);
    }
    findNextBlank(){
        if(this.variable instanceof BlankTerm){
            return {formula:this, index:'variable'};
        }
        if(this.subformula instanceof BlankFormula){
            return {formula:this, index:'subformula'};
        }
        return this.subformula.findNextBlank();
    }
    isComplete(){
        return this.variable.isComplete() && this.subformula.isComplete();
    }
    substitute(substituent,substitutor){
        if(areTheSame(this.variable, substitutor)){
            if(!(substitutor instanceof Variable)){
                alert('Wrong Syntax! Cannot quantify over non-variable.');
            }
            this.variable=substituent;
        }
        this.subformula.substitute(substituent,substitutor);
    }
    deepCopy(){
        return new QuantifierFormula(this.type, this.variable.deepCopy(), this.subformula.deepCopy());
    }
}
class BlankFormula extends Formula{
    constructor(){
        super();
        this.symbol = '\\_';
    }
    freeVariables() {
        return new Set();
    }

    latex(){
        return this.symbol;
    }
    findNextBlank(){
        return null;
    }
    isComplete(){
        return false;
    }
    substitute(substituent,substitutor){
        return;
    }
    deepCopy(){
        return new BlankFormula();
    }
}
class SubstitutionFormula extends Formula{
    constructor(fml, substituent, variable) {
        super();
        this.subformula = fml;
        this.substituent = substituent;
        this.substitutor = variable;
    }
    latex(){
        return this.subformula.latex() + '\\frac{' + this.substituent.latex() + '}{' + this.substitutor.latex() + '}';
    }
    insertAtBlank(filler){
        if(this.subformula instanceof BlankFormula){
            if(!(filler instanceof Formula)){
                alert('Formula expected!');
                return false;
            }
            this.subformula = filler;
            return true;
        }
        if(this.subformula.insertAtBlank(filler)){
            return true;
        }
        if(this.substituent instanceof BlankTerm){
            if(!(filler instanceof Term)){
                alert('Term expected!');
                return false;
            }
            this.substituent = filler;
            return true;
        }
        if(this.substituent.insertAtBlank(filler)){
            return true;
        }
        if(this.substitutor instanceof BlankTerm){
            if(!(filler instanceof Variable)){
                alert('Variable expected!');
                return false;
            }
            this.substitutor = filler;
            return true;
        }
        alert('Formula already built!');
        return false;
    }
    isComplete(){
        return this.subformula.isComplete() && this.substituent.isComplete() && this.substitutor.isComplete();
    }
    deepCopy(){
        return new SubstitutionFormula(this.subformula.deepCopy(), this.substituent.deepCopy(), this.substitutor.deepCopy());
    }
    resolveSubstitution(){
        let copiedFml = this.subformula.deepCopy();
        copiedFml.substitute(this.substituent,this.substitutor);
        return copiedFml;
    }
}
let contradiction = new AtomicFormula(0,'\\bot',[]);
function concat(fml1, type, fml2){
    return new ConcatFormula(fml1, type, fml2);
}
function areTheSame(obj1,obj2){
    return obj1.latex() === obj2.latex();
}
function sameAntecedents(sequent1, sequent2){
    var antecedent1 = sequent1.antecedent;
    var antecedent2 = sequent2.antecedent;
    antecedent1 = antecedent1.map(fml => fml.latex());
    antecedent2 = antecedent2.map(fml => fml.latex());
    if (antecedent1.length !== antecedent2.length){
        return false;
    }
    for(let i = 0; i< antecedent1.length; i++){
        if(antecedent1[i] !== antecedent2[i]){
            return false;
        }
    }
    return true;
}


//Theories
const sleep = ms => new Promise(r => setTimeout(r, ms));
const ruleNames = {
    'assumption' : '\\text{assumption}',
    'monotonicity' : '\\text{monotonicity}',
    'substitution' : '\\frac{\\cdot}{\\cdot}\\text{-elim.}',
    'desubstitution' : '\\frac{\\cdot}{\\cdot}\\text{-intro.}',
    'andElimination' : '\\wedge\\text{-elim.}',
    'andIntroduction' : '\\wedge\\text{-intro.}',
    'orElimination' : '\\vee\\text{-elim.}',
    'orIntroduction' : '\\vee\\text{-intro.}',
    'implicationElimination' : '\\rightarrow\\text{-elim.}',
    'implicationIntroduction' : '\\rightarrow\\text{-intro.}',
    'forallElimination' : '\\forall\\text{-elim.}',
    'forallIntroduction' : '\\forall\\text{-intro.}',
    'existsElimination' : '\\exists\\text{-elim.}',
    'existsIntroduction' : '\\exists\\text{-intro.}',
    'equalElimination' : '=\\text{-elim.}',
    'equalIntroduction' : '=\\text{-intro.}',
    'contradictionElimination' : '\\bot\\text{-elim.}',
    'contradictionIntroduction' : '\\bot\\text{-intro.}'
}
class Sequent{
    constructor(antecedent, succedent, derivedBy, appliedToSequents){
        this.antecedent = antecedent;
        this.succedent = succedent;
        this.derivedBy = derivedBy;
        this.appliedToSequents = appliedToSequents;
        this.length = antecedent.length;
        this.index = proof.length;
    }
    latex(){
        let code = '\\text{}' + this.index.toString() + ':\\ ';
        this.length > 5? code += '\\Gamma\\ ' : code += '';
        for(let i=5; i > 0; i--){
            if(this.length - i >= 0){
                code += this.antecedent[this.length - i].latex();
                if(i===2){
                    code += ',\\ ';
                }
            }
        }
        code += ' \\vdash ';
        code += this.succedent.latex();
        code += '\\ \\text{ by }' + ruleNames[this.derivedBy];
        if(this.appliedToSequents.length >= 1){
            code += '\\text{ at }' + this.appliedToSequents[0];
            if(this.appliedToSequents.length === 2){
                code += '\\text{ and }' + this.appliedToSequents[1];
            }
        }
        return code;
    }
    async insertAtBlank(filler){
        switch(this.derivedBy){
            case 'assumption':
                if(this.antecedent[this.length- 1] instanceof BlankFormula){
                    if(filler instanceof Term){
                        alert('Wrong Syntax! Expecting a formula, not a term.');
                        return;
                    }
                    this.antecedent[this.length- 1] = filler;
                    this.succedent = filler;
                }
                else{
                    this.succedent.insertAtBlank(filler);
                }
                break;
            case 'monotonicity':
                let lastFml = this.antecedent[this.length- 1];
                if(lastFml instanceof BlankFormula){
                    if(filler instanceof Term){
                        alert('Wrong Syntax! Expecting a formula, not a term.');
                        return;
                    }
                    this.antecedent[this.length- 1] = filler;
                }
                else{
                    lastFml.insertAtBlank(filler);
                }
                break;
            case 'desubstitution':
                if(this.isComplete()){
                    alert('The derivation is already complete!');
                    return;
                }
                this.succedent.insertAtBlank(filler);
                if(this.succedent.isComplete()){
                    let oldFml = proof[this.appliedToSequents[0]].succedent;
                    if(!areTheSame(oldFml, this.succedent.resolveSubstitution())){
                        alert('The proposed annotated substitution does not resolve to the prior derived formula!');
                        undoDerivation();
                    }
                }
                break;
            case 'equalIntroduction':
                if(this.succedent.terms[0] instanceof BlankTerm){
                    this.succedent.terms = [filler,filler];
                    return;
                }
                this.succedent.insertAtBlank(filler);
                break;
            case 'forallIntroduction':
                if(this.succedent.variable instanceof BlankTerm){
                    if(!(filler instanceof Variable)){
                        alert('Expecting Variable!');
                        return;
                    }
                    let newFml = this.succedent.subformula.forall(filler);
                    let sequence = [...this.antecedent];
                    sequence.push(newFml);
                    let latexVariables = freeVariablesFromSequence(sequence);
                    if(latexVariables.has(filler.symbol)){
                        alert('The variable to quantify over is not free enough! Note that it is not allowed to appear in any assumption.');
                        return;
                    }
                    this.succedent = newFml;
                }
                break;
            case 'forallElimination':
                if(this.succedent.substituent instanceof BlankTerm){
                    if(!(filler instanceof Term)){
                        alert('Term expected!');
                        return;
                    }
                    this.succedent.substituent = filler;
                }
                else{
                    this.succedent.substituent.insertAtBlank(filler);
                }
                if(this.succedent.substituent.isComplete()){
                    showProof();
                    await sleep(500);
                    let newFml = this.succedent.resolveSubstitution();
                    this.succedent = newFml;
                    break;
                }
                break;
            default:
                this.succedent.insertAtBlank(filler);
                break;
        }
        showProof();
    }
    isAntecedentComplete(){
        let complete = true;
        this.antecedent.forEach(fml => complete = complete && fml.isComplete());
        return complete;
    }
    isComplete(){
        let result = this.isAntecedentComplete() && this.succedent.isComplete();
        return result;
    }
    deepCopy(){
        let copiedAntecedent = [];
        this.antecedent.forEach(fml => copiedAntecedent.push(fml.deepCopy()));
        return new Sequent(copiedAntecedent, this.succedent.deepCopy(), this.derivedBy);
    }
}


let term = null;
let builtTerms = [];

function getBlankTerm(type, symbol){
    let newTerm;
    if(type === 'variable'){
        return new Variable(symbol);
    }
    if(type === 'constant'){
        return new CompositeTerm(0, symbol, []);
    }
    if(type === 'function'){
        let subterms = [];
        let i;
        let arity;

        if(symbol === 'f' || symbol === 'g' || symbol === 'h'){
            arity = document.getElementById(symbol + 'arity').value;
        }
        if(symbol === '+' || symbol === '*'){
            arity = 2;
        }
        for(i=0; i < arity; i++){
            subterms.push(new BlankTerm());
        }
        return new CompositeTerm(arity, symbol, subterms);
    }
    if(type === 'savedterm'){
        return builtTerms[symbol];
    }
}
function termBuilder(type, symbol){
    let blank;
    if(term != null) {
        blank = term.findNextBlank();
        if (blank == null) {
            alert('Term is already built.');
            return;
        }
    }
    let newTerm = getBlankTerm(type,symbol);
    term == null ? term = newTerm : blank.term.subterms[blank.index] = newTerm;
    document.getElementById("latex").innerHTML = '\\(' + term.latex() + '\\)';
    renderMathInElement(document.body);
}

function saveTerm(){
    if(term == null){
        alert('Term is empty!');
        return;
    }
    builtTerms.push(term);
    resetTerm();
    showSaves('term');
    return;
}
function resetTerm(){
    term = null;
    return;
}

async function fetchHtmlAsText(url) {
    return await (await fetch(url)).text();
}
async function loader(name) {
    const contentDiv = document.getElementById("magic");
    contentDiv.innerHTML = await fetchHtmlAsText(name);
    renderMathInElement(document.body);
}

let formula = null;
let builtFormuli = [];
let proof = [];
function showBuilder(name){
    /* document.getElementById('magic').innerHTML='<object id="trickobject" width="100%" height="100%" type="text/html" data="' + name  + '" ></object>';*/
    loader(name);
}

function getBlankFormula(type, symbol){
    switch(type){
        case 'atomic':
            if(symbol === '\\varphi' || symbol === '\\psi' || symbol === '\\chi' || symbol === '\\bot'){
                return new AtomicFormula(0, symbol, []);
            }
            let arity = 2;
            if (symbol === 'P' || symbol === 'Q'){
                arity = document.getElementById(symbol + 'arity').value;
            }
            let terms = [];
            let i;
            for(i=0; i < arity; i++) {
                terms.push(new BlankTerm());
            }
            return new AtomicFormula(arity, symbol, terms);
        case 'negation':
            let newformula = new BlankFormula();
            return newformula.neg();
        case 'concat':
            let blankLeft = new BlankFormula();
            let blankRight = new BlankFormula();
            return concat(blankLeft, symbol, blankRight);
        case 'quantifier':
            let blankTerm = new BlankTerm();
            let blankFormula = new BlankFormula();
            return blankFormula.quantify(symbol, blankTerm);
        case 'savedformula':
            return builtFormuli[symbol];
    }
    return getBlankTerm(type,symbol);
}

function formulaBuilder(type, symbol = null){
    if(type === 'savedformula' && buildingGamma){
        let fml = builtFormuli[symbol];
        gamma.includes(fml)? delete gamma[gamma.indexOf(fml)] : gamma.push(fml);
        showGamma()
        return;
    }
    if(proof.length === 0){
        alert('Do a derivation! Start with the assumption rule with empty Gamma or the =-introduction rule!');
        return;
    }
    let lastSequent = proof[proof.length -1];
    if(lastSequent.isComplete()){
        alert('Apply a derivation rule!');
        return;
    }
    let filler = getBlankFormula(type, symbol);
    lastSequent.insertAtBlank(filler);
    showProof();
}


let listener = {
    rule: null,
    clicked: [],
    saturated(){
        if (this.rule === null){
            return false;
        }
        if (['implicationElimination', 'contradictionIntroduction','equalElimination','andIntroduction','orElimination','existsElimination'].includes(this.rule)) {
            return this.clicked.length >= 2;
        }
        return this.clicked.length >= 1;
    },
    reset(rule){
        this.clicked = [];
        let elements = document.getElementById("rules").getElementsByTagName('*');
        for(let i = 0; i < elements.length; i++){
            let element = elements[i];
            if(element.id === 'assumptioncontent' || element.id === 'andEliminationContent' || element.id === 'orIntroductionContent'){
                if(!(['andEliminationRight', 'orIntroductionRight'].includes(rule))){
                    element.style = 'display:none;';
                }
            }
            if(element.tagName === 'BUTTON'){
                element.classList.remove("active-rule");
            }
        }
        this.rule = null;
    }

};

function selectSequent(index){
    listener.clicked.push(index);
    if(listener.saturated()){
        saturatedCalculus();
    }
}

function saveFormula(){
    if (formula == null || formula.findNextBlank() != null){
        alert('Formula or term not finished!');
        return;
    }
    formula instanceof Formula ? builtFormuli.push(formula) : builtTerms.push(formula);
    resetFormula();
    showSaves('both');
}
function resetFormula(){
    formula = null;
    document.getElementById('latex').innerHTML = '\\( \\_ \\)';
    renderMathInElement(document.getElementById('latex'));
}

let gamma = [];
let buildingGamma = false;

function buildGamma(){
    let newColor;
    let newText;
    buildingGamma ? newColor = 'transparent' : newColor = 'grey';
    buildingGamma ? newText = 'Build Gamma' : newText = 'Building Gamma';
    document.getElementById('gammaBuilder').style.backgroundColor= newColor;
    document.getElementById('gammaBuilder').innerText= newText;
    document.getElementById('gammaBuilder').style.borderRadius = '2px';
    document.getElementById('gammaBuilder').style.border = '1px solid grey';
    buildingGamma = !buildingGamma;
}
function resetGamma(){
    gamma = [];
    showGamma();
}
function showGamma(){
    let latex = '';
    gamma.forEach(fml => latex += ', ' + fml.latex());
    latex = latex.slice(1);
    latex = '\\(\\Gamma = \\{' + latex +  '\\}\\)';
    document.getElementById('Gamma').innerHTML = latex;
    renderMathInElement(document.getElementById('Gamma'));

}

function undoDerivation(){
    proof.pop();
    showProof();
}



function showProof(){
    let code = '';
    for(let i = 0; i < proof.length; i++){
        let sequent = proof[i];
        code += '<p>';
        if(sequent.isComplete()){
            code += '<button class="sqbutton" onclick = \"selectSequent(' + i.toString() + ')\">';
        }
        code +=  '\\(' + sequent.latex() + '\\)';
        if(sequent.isComplete()){
            code += '</button>';
        }
        code += '</p>';
    }
    let proofElement = document.getElementById("proof");
    proofElement.innerHTML =  code;
    renderMathInElement(proofElement);
}


showBuilder('formulabuilder.html');

function calculus(rule){
    listener.reset(rule);
    document.getElementById(rule).classList.add("active-rule");
    if(rule === 'assumption'){
        let content = document.getElementById('assumptioncontent');
        if(content.style.display === 'none'){
            content.style.display = 'block';
        }
        else{
            content.style.display = 'none';
            return;
        }
    }
    if(rule === 'andEliminationLeft'){
        let content = document.getElementById('andEliminationContent');
        if(content.style.display === 'none'){
            content.style.display = 'block';
        }
        else{
            content.style.display = 'none';
        }
    }
    if(rule === 'orIntroductionLeft'){
        let content = document.getElementById('orIntroductionContent');
        if(content.style.display === 'none'){
            content.style.display = 'block';
        }
        else{
            content.style.display = 'none';
        }
    }
    if(rule === 'equalIntroduction'){
        let term = new BlankTerm();
        proof.push(new Sequent([], new AtomicFormula(2, '=', [term, term]), 'equalIntroduction', []));
        showProof();
        return;
    }
    listener.rule = rule;
    showProof();
}

function freeAssumption(){
    proof.push(new Sequent([new BlankFormula()], new BlankFormula(), 'assumption', []));
    showProof();
    listener.reset();
}

function saturatedCalculus(){
    var fstSequent = proof[listener.clicked[0]];
    var fstFml = fstSequent.succedent;
    var fstAntecedent = fstSequent.antecedent;
    if(listener.clicked.length >= 2){
        var sndSequent = proof[listener.clicked[1]];
        var sndFml = sndSequent.succedent;
        var sndAntecedent = sndSequent.antecedent;
        if(listener.rule !== 'existsElimination' && !sameAntecedents(fstSequent,sndSequent)){
            alert('Error! The assumptions differ!')
            return;
        }
    }
    switch(listener.rule){
        case 'assumption':
            var antecedentCopy = [...fstAntecedent];
            let newBlank = new BlankFormula();
            antecedentCopy.push(newBlank);
            proof.push(new Sequent(antecedentCopy, newBlank, 'assumption', listener.clicked));
            break;
        case 'monotonicity':
            var antecedentCopy = [...fstAntecedent];
            antecedentCopy.push(new BlankFormula());
            proof.push(new Sequent(antecedentCopy, fstFml, 'monotonicity', listener.clicked));
            break;
        case 'substitution':
            if(!(fstFml instanceof SubstitutionFormula)){
                alert('This rule resolves an annotated substitution, but the selected derived formula has no annotation!');
                break;
            }
            proof.push(new Sequent(fstAntecedent, fstFml.resolveSubstitution(), 'substitution', listener.clicked));
            break;
        case 'desubstitution':
            proof.push(new Sequent(fstAntecedent, new SubstitutionFormula(new BlankFormula(), new BlankTerm(), new BlankTerm()), 'desubstitution', listener.clicked));
            break;
        case 'implicationIntroduction':
            if (fstSequent.length === 0){
                alert('The implication introduction rule requires a derivation with an assumption!');
                break;
            }
            var antecedentCopy = [...fstAntecedent];
            var lastAssumption = antecedentCopy[antecedentCopy.length- 1];
            antecedentCopy.splice(antecedentCopy.length - 1,1);
            proof.push(new Sequent(antecedentCopy, lastAssumption.implies(fstFml),'implicationIntroduction', listener.clicked));
            break;
        case 'implicationElimination':
            if(!(sndFml instanceof ConcatFormula) || sndFml.type !== 'implication'){
                alert('The second selected formula is not an implication! No Modus Ponens possible.');
                break;
            }
            if(!areTheSame(fstFml,sndFml.subformulaLeft)){
                alert('The first derived formula does not agree with the premise of the implication!');
                break;
            }
            proof.push(new Sequent(fstAntecedent, sndFml.subformulaRight, 'implicationElimination', listener.clicked));
            break;
        case 'contradictionIntroduction':
            if(!(sndFml instanceof NegFormula)){
                alert('The second derived formula is not a negation!');
                break;
            }
            if(!areTheSame(sndFml.subformula,fstFml)){
                alert ('The second derived formula is not the negation of the first!');
                break;
            }
            proof.push(new Sequent(fstAntecedent, contradiction, 'contradictionIntroduction', listener.clicked));
            break;
        case 'contradictionElimination':
            if(!(areTheSame(fstFml, contradiction))){
                alert('The derived formula is not the falsum!');
                break;
            }
            if(fstAntecedent.length === 0){
                alert('You derived a contradiction without any assumption! That should be impossible. Please send me an email with your derivations!');
                break;
            }
            var lastAssumption = fstAntecedent[fstAntecedent.length -1];
            if(!(lastAssumption instanceof NegFormula)){
                alert('The last assumption is not a negation!');
                break;
            }
            var antecedentCopy = [...fstAntecedent];
            antecedentCopy.splice(antecedentCopy.length -1, 1);
            proof.push(new Sequent(antecedentCopy, lastAssumption.subformula, 'contradictionElimination', listener.clicked));
            break;
        case 'forallIntroduction':
            proof.push(new Sequent(fstAntecedent, fstFml.forall(new BlankTerm()), 'forallIntroduction', listener.clicked));
            break;
        case 'forallElimination':
            proof.push(new Sequent(fstAntecedent, fstFml.subformula.addSubstitution(new BlankTerm(), fstFml.variable), 'forallElimination', listener.clicked));
            break;
        case 'equalElimination':
            if(!(sndFml instanceof AtomicFormula) || !(sndFml.symbol === '=')){
                alert('The second derived formula must be an equality!');
                break;
            }
            if(!(fstFml instanceof SubstitutionFormula)){
                alert('The first derived formula must have an annotated substitution!');
                break;
            }
            if(!(areTheSame(fstFml.substituent, sndFml.terms[1]))){
                alert('The term in the annotated substitution of the first derived formula does not match the second term in the equality of the second derived formula!');
                break;
            }
            var newFml = fstFml.deepCopy();
            newFml.substituent = sndFml.terms[0];
            proof.push(new Sequent(fstAntecedent, newFml, 'equalElimination', listener.clicked));
            break;
        case 'andIntroduction':
            proof.push(new Sequent(fstAntecedent, fstFml.conjunct(sndFml), 'andIntroduction', listener.clicked));
            break;
        case 'andEliminationLeft':
            if(!(fstFml instanceof ConcatFormula) || !(fstFml.type === 'conjunction')){
                alert('The derived formula must be a conjunction!');
                break;
            }
            proof.push(new Sequent(fstAntecedent, fstFml.subformulaLeft, 'andEliminationLeft', listener.clicked));
            break;
        case 'andEliminationRight':
            if(!(fstFml instanceof ConcatFormula) || !(fstFml.type === 'conjunction')){
                alert('The derived formula must be a conjunction!');
                break;
            }
            proof.push(new Sequent(fstAntecedent, fstFml.subformulaRight, 'andEliminationLeft', listener.clicked));
            break;
        case 'orIntroductionLeft':
            proof.push(new Sequent(fstAntecedent, fstFml.disjunct(new BlankFormula()), 'orIntroductionLeft', listener.clicked));
            break;
        case 'orIntroductionRight':
            proof.push(new Sequent(fstAntecedent, (new BlankFormula()).disjunct(fstFml), 'orIntroductionRight', listener.clicked));
            break;
        case 'orElimination':
            if(!(fstFml instanceof ConcatFormula) || fstFml.type !== 'disjunction'){
                alert('First derived formula must be a disjunction!');
                break;
            }
            if(!(sndFml instanceof NegFormula) || !(areTheSame(fstFml.subformulaLeft, sndFml.subformula))){
                alert('The second derived formula must be the negation of the first formula in the derived disjunction!');
                break;
            }
            proof.push(new Sequent(fstAntecedent, fstFml.subformulaRight, 'orElimination', listener.clicked));
            break;
        case 'existsIntroduction':
            if(!(fstFml instanceof SubstitutionFormula)){
                alert('The derived formula must have annotated substitution!');
                break;
            }
            proof.push(new Sequent(fstAntecedent, fstFml.subformula.exists(fstFml.substitutor), 'existsIntroduction', listener.clicked));
            break;
        case 'existsElimination':
            if(!(fstFml instanceof QuantifierFormula) || fstFml.type !== 'exists'){
                alert('The first derived formula must be an existential formula!');
                break;
            }
            if(sndAntecedent.length === 0){
                alert('The second derived formula must have an assumption!');
                break;
            }
            var lastAssumption = sndAntecedent[sndAntecedent.length- 1];
            var antecedentCopy = [...sndAntecedent];
            antecedentCopy.pop();
            if(!sameAntecedents(fstSequent, new Sequent(antecedentCopy))){
                alert('Error! The assumptions do not match properly! The second derivation is supposed to have exactly one assumption more!');
                break;
            }
            if(fstFml.subformula.freeVariables().has(fstFml.variable.latex())){
                let y;
                let foundY = false;
                for(let freeVariableSymbol of lastAssumption.freeVariables()){
                    let freeVariable = new Variable(freeVariableSymbol);
                    if(areTheSame((new SubstitutionFormula(lastAssumption, fstFml.variable, freeVariable)).resolveSubstitution(), fstFml.subformula)){
                        y = freeVariable;
                        foundY = true;
                        break;
                    }
                }
                if(!foundY){
                    alert('Could not find a matching variable!');
                    break;
                }
                var sequence = [...fstAntecedent];
                sequence.push(fstFml);
                sequence.push(sndFml);
                if(freeVariablesFromSequence(sequence).has(y.latex())){
                    alert('The relevant free variable in the assumption of the second derivation is not free enough! It cannot be a free variable in any of the assumptions in the first derivation, nor in any of the two derived formulas.');
                    break;
                }
            }
            else{
                if(!areTheSame(lastAssumption, fstFml.subformula)){
                    alert('The assumption in the second derivation does not appear to be an instance of the formula quantified over in the first derivation!');
                    break;
                }
            }
            proof.push(new Sequent(fstAntecedent, sndFml, 'existsElimination', listener.clicked));
            break;
    }
    listener.reset();
    showProof();
}






/*
let x = new Variable('x');
let y = new Variable('y');
let t = new Variable('t');
let s = new Variable('s');
let phi = new AbstractFormula('\\varphi');
let psi = new AbstractFormula('\\psi');
let emptyFormula = new AbstractFormula('');
let gamma = new Theory('\\Gamma', []);
let emptysequent = new Sequent(gamma,[], emptyFormula);
let phisequent = new Sequent(gamma, [], phi);
let psisequent = new Sequent(gamma, [], psi);
let assumtpionRule = new Derivation([emptysequent], phisequent);

console.log('Monotonicity: \n' + (new Derivation([getGammaSequent(phi)], new Sequent(gamma, [psi], phi))).latex());
console.log('Assumption Rule: \n' + assumtpionRule.latex());
console.log('-> Intro: \n' + (new Derivation([new Sequent(gamma, [phi], psi)], getGammaSequent(concat(phi, 'implication', psi)))).latex());
console.log('Modus Ponens: \n' + (new Derivation([phisequent, getGammaSequent(concat(phi, 'implication', psi))], psisequent)).latex());
console.log('bot Intro: \n' + (new Derivation([phisequent, getGammaSequent(phi.neg())], getGammaSequent(contradiction))).latex());
console.log('bot Elim: \n' + (new Derivation([new Sequent(gamma, [phi.neg()], contradiction)], phisequent)).latex());
console.log('forall intro: \n' + (new Derivation([getGammaSequent(phi.substitute(x, y))], getGammaSequent(phi.forall(x)))).latex());
console.log('forall elim: \n' + (new Derivation([getGammaSequent(phi.forall(x))], getGammaSequent(phi.substitute(x, t)))).latex());
console.log('equal intro: \n' + (new Derivation([], getGammaSequent(new AtomicFormula(2, '=', [t, t])))).latex());
console.log('equal elim: \n' + (new Derivation([getGammaSequent(phi.substitute(x, t)), getGammaSequent(new AtomicFormula(2, '=', [s, t]))], getGammaSequent(phi.substitute(x, s)))).latex());

console.log('and intro \n' + (new Derivation([phisequent, psisequent], getGammaSequent(concat(phi, 'conjunction', psi)))).latex());
console.log('and elim \n' + (new Derivation([getGammaSequent(concat(phi, 'conjunction', psi))], phisequent)).latex());
console.log('or intro: \n' + (new Derivation([phisequent], getGammaSequent(concat(phi, 'disjunction', psi)))).latex());
console.log('or elim: \n' + (new Derivation([getGammaSequent(concat(phi, 'disjunction', psi)), getGammaSequent(phi.neg())], psisequent)).latex());
console.log('exists intro: \n' + (new Derivation([getGammaSequent(phi.substitute(x, t))], getGammaSequent(phi.exists(x)))).latex());
console.log('exists elim: \n' + (new Derivation([getGammaSequent(phi.exists(x)), new Sequent(gamma, [phi.substitute(x, t)], psi)], psisequent)).latex());

document.getElementById('proverlatex').innerHTML = '\\(' + new Derivation([getGammaSequent(phi)], new Sequent(gamma, [psi], phi)).latex() + '\\)';
*/
