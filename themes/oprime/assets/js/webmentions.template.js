async function displayWebmentions() {
    let endpoint = '{{ .Param "indieweb.endpoints.webmentiond" }}';
    let target = window.location.href;
    let resp = await fetch(`${endpoint}get?target=${target}`);
    let data = await resp.json();
    let mentionsListView = document.querySelector('#mentionslist');
    let list = document.createElement('ul');
    data.forEach(mention => {
        let listItem = document.createElement('li');
        listItem.innerHTML = `<a href="${mention.source}" target="_blank" rel="nofollow noopener noreferrer">${mention.author_name ? mention.author_name : mention.source}</a>${mention.title ? ` <i>${mention.title}</i>` : ''}`;
        list.appendChild(listItem);
    });
    mentionsListView.innerHTML = ``;
    mentionsListView.appendChild(list);
}

displayWebmentions();