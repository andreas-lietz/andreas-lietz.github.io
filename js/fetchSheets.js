function addSheets(lenaWebsite){
    //console.log(lenaWebsite);
    const parser = new DOMParser();
    let lenaDocument = parser.parseFromString(lenaWebsite, 'text/html');
    let downloads = lenaDocument.getElementsByClassName("wpContentElementDownloadButton");
    console.log(downloads)
    let sheetsHtml = "<p> If you are using the right browser, the exercise sheets will be mirrored below.</p>";
    for(let i=0; i<downloads.length; i++){
        let download = downloads.item(i);
        console.log(download.href);
        sheetsHtml += "<p><a href=\"" + download + "\"> Session " + (i + 1) + " Sheet </a></p>";
    }
    console.log(sheetsHtml);
    document.getElementById("exercises").innerHTML = sheetsHtml;
}
//"https://www.tuwien.at/mg/dmg/lenawallner/lehre"

fetch("https://crossorigin.me/https://www.tuwien.at/mg/dmg/lenawallner/lehre")
    .then((response) => {
        if(!response.ok){
            alert("Fetching URL failed!");
        }
        response.text().then((data) => {addSheets(data);});
  });