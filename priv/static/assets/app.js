function init() {
    document.getElementById('search').addEventListener("submit", function(event) {
        event.preventDefault();
        answer(event);
    }, false);
}

function answer(_event) {
    question = document.getElementById('question').value;
    questionEncoded = encodeURIComponent(question);
    fetchOptions = {
        method: 'GET',
        mode: 'no-cors',
        headers: {
            'Accept': 'application/json'
        }
    };
    fetch('/answer/' + questionEncoded, fetchOptions)
    .then(response => response.json())
    .then(json => render_answer(json));
}

function render_answer(json) {
    answerElem = document.getElementById('answer');
    removeAllChildren(answerElem);
    if (json.answers.length == 0) {
        answerNode = document.createElement('div');
        answerNode.setAttribute('class', 'simple');
        answerNode.appendChild(document.createTextNode("¯\\_(ツ)_/¯"));
    } else {
        answerNode = answerToNode(json.answers[0]);
    }
    answerElem.appendChild(answerNode);
}

function answerToNode(answer) {
    var node;
    switch(answer.type) {
        case "text":
            node = document.createElement('div');
            node.setAttribute('class', 'simple');
            node.appendChild(document.createTextNode(answer.answer.text));
            break;
        case "hash":
            node = document.createElement('div');
            node.setAttribute('class', 'simple');
            node.appendChild(document.createTextNode(answer.answer.hash));
            break;
        case "conversion_result":
            const a = answer.answer;
            var txt = a.unit_from_number + " " + a.unit_from_name + " = " + a.unit_to_number + " " + a.unit_to_name
            node = document.createElement('div');
            node.setAttribute('class', 'simple');
            node.appendChild(document.createTextNode(txt));
            break;
        case "wiki":
            node = document.createElement('ul');
            node.setAttribute('class', 'wiki');
            answer.answer.forEach(function(result) {
                var itemNode = document.createElement('li');
                var linkNode = document.createElement('a');
                linkNode.setAttribute('href', result.url);
                linkNode.appendChild(document.createTextNode(result.title));
                var descNode = document.createElement('span');
                descNode.innerHTML = result.snippet;
                itemNode.appendChild(linkNode);
                itemNode.appendChild(descNode);
                node.appendChild(itemNode);
            });
            break;
        case "definition":
            node = document.createElement('ul');
            node.setAttribute('class', 'definition');
            answer.answer.forEach(function(result) {
                var wordNode = document.createElement('li');
                var wordHeader = document.createElement('header');
                var wordText = document.createElement('span');
                wordText.setAttribute('class', 'word');
                wordText.textContent = result.word;
                // phonetics
                var phoneticsList = document.createElement('ul');
                phoneticsList.setAttribute('class', 'phonetics');
                result.phonetics.forEach(function(p) {
                    var phoneticsItem = document.createElement('li');
                    var phoneticsItemLink = document.createElement('a');
                    phoneticsItemLink.setAttribute('href', p.audio);
                    phoneticsItemLink.appendChild(document.createTextNode(p.text));
                    phoneticsItem.appendChild(phoneticsItemLink);
                    phoneticsList.appendChild(phoneticsItem);
                });
                wordHeader.appendChild(wordText);
                wordHeader.appendChild(phoneticsList);
                wordNode.appendChild(wordHeader);
                // meanings
                var meaningsList = document.createElement('ul');
                meaningsList.setAttribute('class', 'meanings');
                result.meanings.forEach(function (m) {
                    var meaningsItem = document.createElement('li');
                    meaningsItem.appendChild(document.createTextNode(m.part_of_speech));
                    var defsList = document.createElement('ul');
                    m.definitions.forEach(function(d) {
                        var defsItem = document.createElement('li');
                        defsItem.appendChild(document.createTextNode(d.definition));
                        if (d.example !== undefined) {
                            var defsExample = document.createElement('div');
                            defsExample.appendChild(document.createTextNode("example: " + d.example));
                            defsItem.appendChild(defsExample);
                        }
                        defsList.appendChild(defsItem);
                    });
                    meaningsItem.appendChild(defsList);
                    meaningsList.appendChild(meaningsItem);
                });
                wordNode.appendChild(meaningsList);
                node.appendChild(wordNode);
            });
            break;
        case "redirect":
            node = document.createElement('div');
            node.setAttribute('class', 'simple')
            var url = answer.answer.url;
            var source = answer.answer.source;
            var query  = answer.answer.query;
            var linkNode = document.createElement('a');
            linkNode.setAttribute('href', url);
            linkNode.appendChild(document.createTextNode('Search ' + source + ' for ' + query));
            node.appendChild(linkNode);
            window.open(url, '_blank');
            break;
        default:
            node = document.createTextNode("");
    };
    return node;
}

function removeAllChildren(node) {
    while (node.firstChild) {
        node.removeChild(node.lastChild)
    }
}

window.onload = (_event) => {
    init();
};
