function addSheets(lenaWebsite){
    console.log(lenaWebsite);
    const parser = new DOMParser();
    //let lenaDocument = parser.parseFromString(lenaWebsite.contents, 'text/html');
    let lenaDocument = parser.parseFromString(lenaWebsite, 'text/html');
    let downloads = lenaDocument.getElementsByClassName("wpContentElementDownloadButton");
    console.log(downloads)
    let sheetsHtml = "<p> The exercise sheets are mirrored below.</p>";
    for(let i=0; i<downloads.length; i++){
        let download = downloads.item(i);
        console.log(download.href);
        sheetsHtml += "<p><a href=\"" + download + "\"> Session " + (i + 1) + " Sheet </a></p>";
    }
    console.log(sheetsHtml);
    document.getElementById("exercises").innerHTML = sheetsHtml;
}
//"https://www.tuwien.at/mg/dmg/lenawallner/lehre"

fetch('https://corsproxy.io/?' + encodeURIComponent('https://www.tuwien.at/mg/dmg/lenawallner/lehre'))
    .then((response) => {
        if(!response.ok){
            alert("Fetching URL failed!");
        }
        response.text().then((data) => {addSheets(data);});
  });
