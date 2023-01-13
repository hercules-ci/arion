{ lib }:
let

  link = url: text: ''[${text}](${url})'';

  dockerComposeRef = fragment:
    ''See ${link "https://docs.docker.com/compose/compose-file/#${fragment}" "Docker Compose#${fragment}"}'';

in
{
  inherit
    dockerComposeRef
    link
    ;
}
