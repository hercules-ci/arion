{ lib }:
let

  link = url: text:
    ''link:${url}[${text}]'';

  dockerComposeRef = fragment:
    ''See ${link "https://docs.docker.com/compose/compose-file/#${fragment}" "Docker Compose#${fragment}"}'';

in
{
  inherit
    dockerComposeRef
    link
    ;
}
