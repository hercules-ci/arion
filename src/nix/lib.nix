{ lib }:
let

  link = url: text: ''[${text}](${url})'';

  serviceRef = fragment:
    ''See ${link "https://docs.docker.com/compose/compose-file/05-services/#${fragment}" "Docker Compose Services #${fragment}"}'';

  networkRef = fragment:
    ''See ${link "https://docs.docker.com/compose/compose-file/06-networks/#${fragment}" "Docker Compose Network #${fragment}"}'';

in
{
  inherit
    link
    networkRef
    serviceRef
    ;
}
