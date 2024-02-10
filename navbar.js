async function fetchHtmlAsText(url) {
    return await (await fetch(url)).text();
}

async function loader() {
    const contentDiv = document.getElementById("includeNavbar");
    contentDiv.innerHTML = await fetchHtmlAsText("navbar.html");
    var path = window.location.pathname;
    var btn = document.getElementById(path);
    btn.style="background-color: #ccc; color: black; border: 2px solid #555555;"
    //renderMathInElement(document.body);
}

loader();