{ lib }:
let

  link = url: text: ''[${text}](${url})'';

  composeSpecRev = "55b450aee50799a2f33cc99e1d714518babe305e";

  serviceRef = fragment:
    ''See ${link "https://github.com/compose-spec/compose-spec/blob/${composeSpecRev}/05-services.md#${fragment}" "Compose Spec Services #${fragment}"}'';

  networkRef = fragment:
    ''See ${link "https://github.com/compose-spec/compose-spec/blob/${composeSpecRev}/06-networks.md#${fragment}" "Compose Spec Networks #${fragment}"}'';

  secretRef = fragment:
    ''See ${link "https://github.com/compose-spec/compose-spec/blob/${composeSpecRev}/09-secrets.md#${fragment}" "Compose Spec Secrets #${fragment}"}'';

in
{
  inherit
    link
    networkRef
    serviceRef
    secretRef
    ;
}
