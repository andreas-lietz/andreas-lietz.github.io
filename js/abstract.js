function openAbstract(idNumber){
    let abstractElement = document.getElementById("abstract" + idNumber);
    abstractElement.style.display == "none"? abstractElement.style.display = "block" : abstractElement.style.display = "none";
}