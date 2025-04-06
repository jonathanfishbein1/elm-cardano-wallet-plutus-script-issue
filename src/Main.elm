port module Main exposing (..)

import Browser
import Bytes.Comparable
import Cardano
import Cardano.Address
import Cardano.Cip30
import Cardano.Data
import Cardano.Gov
import Cardano.Metadatum
import Cardano.MultiAsset
import Cardano.Script
import Cardano.Transaction
import Cardano.Utxo
import Cardano.Value
import Cbor.Decode
import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Natural
import Task
import Toop


network : Cardano.Address.NetworkId
network =
    Cardano.Address.Testnet


referenceWalletAddressText =
    "addr_test1qp5vzne98n43jd9w335udqpkyua5yk7a8jrfslzdqc0pr8xqgwp4c5ekhct3tyygcf9lrxe6f9csdv757swftg0gvras28x5jz"


maybeReferenceWalletAddress : Maybe Cardano.Address.Address
maybeReferenceWalletAddress =
    Cardano.Address.fromString referenceWalletAddressText


metadataScriptString : String
metadataScriptString =
    "5918e401010032323232323232323232323232323232323232232329593333330142255333573466e1d20000021132593300122800c00400a002001486400644a66464645266666603c44aa666ae68cdc3a400000422b26601445001800801400400190c800c88c964c00400e42b26601e45001800801400400390c800c8964c01400a42b26602645001800801400400290c800c8964cc07c08400a42b26602e45001800801400400290c800c88c964c00400e42b26603845001800801400400390c800c88c8c964cc004c00401c01242b26604445001800801400400490c800c8964cc0b808000a42b26604c45001800801400400290c800c8964ccc0e00d803c00a42b26605445001800801400400290c800c8964cccccc10c8954ccd5cd19b87480000084564cc0bc8a0030010028008003219001912c999999824112a999ab9a3370e900000108ac9981a114006002005001000643200322593304901d00290ac9981c1140040080052159304a00190c00600200700123002460048c0088c00823002280114008a004500200290ac99819914004008005215933333304a2255333573466e1d200000211593303622800c00400a002000c86400644b26609603e00521593303a22800801000a42b2609800321800c00400e002460048c0091801118010460045002280114008a00400321800c00400e002460048c0091801118010460045002280114008a00400521593302e22800c00400a002001486400644b26607402400521593303222800c00400a002001486400644b2666088466666609844a64a666ae68cdc3a400000622b2660724500100200148564c06c00643001801800913002499130014982260029312992999ab9a3370e900100208ac9981d1140040080072159304500190c00601200244c0092644c00526089800a4c4a64a666ae68cdc3a400800a22b2660764500100200248564c12400643001803800913002499130014982260029312992999ab9a3370e900300308ac9981e11400600200500100164320032259302500290ac998201140040080052159303c00190c00600200700122600c9322600a93226008931130014982260029312992999ab9a3370e900400388ac9981e91400400800d2159303a00190c00601600244c0092644c00526089800a4c4aa666ae68cdc3a401400e22b26607a45001800801400400690c800c8964c09800a42b2660824500100200148564c0f000643001802801c0048c0091801230022300208c008a004500228011400800411400a42b26606c45001800801400400290c800c8964cc06412400a42b26607445001800801400400290c800c8964c07800a42b26607c45001800801400400290c800c8964ccc1400f4cc1401508cccccc1608954ccd5cd19b87480000084600300208aa999ab9a3370e900100108c0060081155333573466e1d200400211800c00023002280114008a004500200100290ac99821114006002005001000a43200322593304e03f00290ac99823114006002005001000a432003223259300100390ac998259140040080072159300300190c00600207f03b81bc0ca059028812408203901880a4042019008801c0048c009180123002182b817a300246004460048c0088c009180111801230022300246004460048c0088c009180111801230022300246004460048c008605e002a0668c0091801140bd1801230022300246004460048c008602c466666604844aa666ae68cdc3a400000422b26602045001800801400400190c800c8964c07400a42b2660284500100200148564c08000643001800801c0048c0091801230022300208c008a004500228011400800518010460045002280114008a00400d21593300922800c00400a002003486400644b26601845001002000c8564cccccc08c894c94ccd5cd19b874800000c4564cc0408a00200400290ac9a812800c860030030012260049322600293044c0052625325333573466e1d200200411593301122800c00400a002001c86400644b2603c00521593301522800801000a42b26604804c00321800c02400e00244c0192644c0152644c0112622600293044c0052625325333573466e1d200400511593301222800801001242b2604000321800c01c0048980124c89800a4c11300149894c94ccd5cd19b87480180184564cc04c8a003001002800800b219001912c9a81580148564cc05c8a00200400290ac9809800c86003001003800913006499130054991300449889800a4c11300149894c94ccd5cd19b874802001c4564cc0508a00200400690ac9808800c8600300b0012260049322600293044c00526255333573466e1d200a00711593301422800c00400a002003486400644b26a05800521593301822800801000a42b2602600321800c01400e002460048c0091801118010460045002280114008a00400321800c00401a00900123002460044600480088cccccc0748954ccd5cd19b87480000084564cc0248a003001002800800321900191192c9800801c8564cc0388a0030010028008007219001912c980f00148564cc0488a00200400290ac999999814912992999ab9a3370e900000188ac9980b114006002005001000a43200322593302702c00290ac9980d114006002005001000a43200322593301d22800801000642b266058a06400321800c02401a00900122601293226010931130054991300449889800a4c11300149894c94ccd5cd19b87480080104564cc05c8a0030010028008007219001912c9981401680148564cc06c8a00200400290ac999999819112a999ab9a3370e900000108ac9980f11400600200500100064320032259301500290ac998111140040080052159301700190c00600200700123002460048c0088c00823002280114008a004500200190c00600200700122600c9322600a93226008931130014982260029312992999ab9a3370e900200288ac9980c1140060020050010012432003225933302a02800f00290ac9980e11400400800521593302b503100190c00601600700122600c9322600a93226008931130014982260029312992999ab9a3370e900300308ac9980c91400400800b21593302802d00190c00600e00244c0092644c00526089800a4c4a64a666ae68cdc3a401000e22b26603445001800801400400690c800c8964cc0ac0c000a42b26603c45001800801400400290c800c8964cc0a80b800a42b26604445001800801400400290c800c8964ccc0d00c806400a42b26604c450010020014856564cccccc0f48954ccd5cd19b87480000084564cc0a48a0030010028008003219001912c981000148564cc0b48a00200400290ac9811000c86003001003800918012300246004460041180114008a00450022801000c86400644aa666ae68cdc3a400000223002118009981800100091801243001806805c01e00700122601c9322601a93226018931130094991300849889802a4c8980224c44c00526089800a4c4a64a666ae68cdc3a401401022b26603645001800801400400790c800c8964cc0b00c400a42b26603e4500100200148564cccccc0d88954ccd5cd19b87480000084564cc0888a00200400190ac99818a81b800c860020028c00918010460045002280114008a00400321800c01400e00244c0192644c0152644c0112622600293044c00526255333573466e1d200c00811800c00823002280114008a004500200190c00600200f003800918012300246004460048c008a0428c00823002280114008a0045002001233333301c225325333573466e1d200000311593300922800c00400a002001486400644b2603200521593300d22800801000a42b266038a04600321800c03400e00244c0192644c0152644c0112622600293044c0052625325333573466e1d200200411593300a22800c00400a002001c86400644b2603400521593300e22800801000a42b26603aa04800321800c04c00e00244c0192644c0152644c0112622600293044c005262532325333573466e1d200400611593300c22800c00400a002002c86400644b2603800521593301022800801000a42b2600e00321800c00c00e00244c0192644c0152644c0112622600293044c0052625325333573466e1d200600711593300d22800c00400a002003486400644b2603a00521593301122800c00400a002001486400644b2601400521593301522800801000a42b26a05600321800c02c01e0070012260149322601293226010931130054991300449889800a4c11300149894c94ccd5cd19b87480200204564cc0388a003001002800800f219001912c980f00148564cc0488a00200400290ac9a814000c86003009003800913006499130054991300449889800a4c11300149894c94ccd5cd19b87480280244564cc03c8a00200400890ac980e800c860030150012260049322600293044c0052625325333573466e1d200c00a11593301022800c00400a002004c86400644b2604000521593301422800801000a42b26a05400321800c04400e00244c0192644c0152644c0112622600293044c0052625325333573466e1d200e00b11593301122800c00400a00200548640064464b2600200721593301622800801000e42b2600600321800c01400e00244c01d2644c0192644c01526281422600293044c0052625325333573466e1d201000c11593301222800c00400a002005c86400644b26a05200521593301622800801000a42b26a05800321800c01c00e00244c0192644c0152644c0112622600293044c0052625325333573466e1d201200d11593301322800c00400a002006486400644b2604600521593301722800801000a42b2604a00321800c00400e00244c0192644c0152644c0112622600293044c00526255333573466e1d201400d11593301322800801003242b2604200321800c03c0048c0091801046004466666604244a64a666ae68cdc3a400000622b26601c4500100200148564d408c0064300180080091300249913001498226002931299192999ab9a3370e900100288ac998081140040080092159300300190c00600a00244c0092644c00526089800a4c4aa666ae68cdc3a400800a22b26602045001800801400400490c800c8964d409c00a42b2660284500100200148564c01c00643001801801c0048c0091801230022300208c0088cccccc094894c94ccd5cd19b874800000c4564cc0488a00200400290ac9810000c860030010012260049322600293044c00526255333573466e1d200200311800c00822aa666ae68cdc3a4008006230018020460045002280114008a0040025002280114008a0040025002280114008a004002466666603644a64a666ae68cdc3a400000622b2660104500100200148564c05800643001800800913002499130014982260029312992999ab9a3370e900100208ac998049140040080072159301700190c00600600244c0092644c00526089800a4c4aa666ae68cdc3a400800822b26601245001002001c8564d407800643001802800918012300208c008a004500228011400800460048c008602f0021180114008a00450022801001c860060028c0052411563616e6e6f74206465636f646520636f6e746578740024600466006921166572726f72206465636f64696e6720636f6e746578740080148c00a4c8cc0066400a4440029140052401176661696c656420746f206578747261637420646174756d004a002921176661696c656420746f206578747261637420646174756d0048a0029201176661696c656420746f206578747261637420646174756d004a002921176661696c656420746f206578747261637420646174756d00489640064300300146002921176661696c656420746f206578747261637420646174756d0025001491176661696c656420746f206578747261637420646174756d0008cc00964cccccc05c8954ccd5cd19b874800000844c964cc0048a0030010028008005219001912c99999980e912a999ab9a3370e900000108ac998031140060020050010006432003223259300100390ac99805914006002005001000e4320032259300500290ac99807914006002005001000a432003223259300100390ac9980a114006002005001000e4320032259300500290ac9980c114006002005001000a43200322593302c503300290ac9980e11400400800521593302e23333330372255333573466e1d200000211593302022800801000642b2603000321800800a3002460041180114008a00450022801000800c86003001014808402e00f003800918012300246004460048c0088c00918010c08c02518012300223002460045020460041180114008a0045002280100148564cc0148a0030010028008005219001912c9a81000148564cc0248a00200400290ac980c800c86003001007801c0048c009180123002230024600446004301a80108c008a004500228011400800643003001460029201166661696c656420746f206465636f646520646174756d000964c8cccccc0648954ccd5cd19b87480000084600300008c008a0045002280114008006401244400490998022ccc802c88800d2222222222222222008460029210c7a65726f207369676e657273004885640063003002488600292110746f6f206d616e79207369676e6572730004b33320072220014880092333573466e3c008006001002940091801c0011800a481176d697373696e67207265717569726564207369676e657200230014911c63616e6e6f7420646573657269616c697a652072656465656d6572200008a400a500100148c0080052330040018014a00012c800c52689802a490350543500112c800c6001133573800500218091112a999ab9a3370e900000088980224903505433001155333573466e200052000113300333702900000119b814800000444ca00266e1000c00666e100080046600a004002464c66ae700060006020444aa666ae68cdc3a400000222004226600600266e180080048c8ccccc03c8a00450020012801140094cc040884cc046003000112c980280148564d4c0144800400a4300180180140048c0091328011a9802090008008480048cccccc0348954ccd5cd19b874800000844c964cc0048a0030010028008005219001912c9a80880148564cc0148a00200400290ac9a80a000c8600300100380091801230024600446004301080108c008a00450022801140080048cccccc0308954ccd5cd19b874800000844c964cc0048a0030010028008005219001912c980400148564cc0148a00300100280080052190019119192c998009800a80b80248564cc02c8a0030010028008009219001912c99999980e112a999ab9a3370e900000108c006000115325333573466e1d200200311593301122800801000a42b2601400321800c0140048980124c89800a4c113001498954ccd5cd19b874801000c4564cc0448a00200400290c006006002460041180114008a0045002280100148564cc03c8a00200400290ac9980b004000c8600300100d803c00e002460048c009180111801230021808000a80a230024600446004300f80108c008a00450022801140080048cccccc02c8954ccd5cd19b874800000844c964cc0048a0030010028008005219001912c980400148564cc0148a00200400290ac99805919999980a112992999ab9a3370e900000188ac998051140040080052159300f00190c00600200244c0092644c00526089800a4c4aa666ae68cdc3a400400622b26601445001800801400400290c800c88c964c00400e42b26601e45001800801400400390c800c8964c01400a42b2660264500100200148564c01c00643001801803c00e002460048c00918011180123002280ca300208c008a004500228011400800400643001800801c0048c0091801230022300218074008460045002280114008a004002466666601444a64a666ae68cdc3a400000622b266601d00222800801000a42b26a01800321800c0040048980124c89800a4c113001498954ccd5cd19b874800800c4564ccc03a0044500100200148564d403000643001801800918012300208c008a004500228011400800488cccccc0288954ccd5cd19b87480080084600300208aa999ab9a3370e900000108ac999806c0088a00200400190ac9802800c8600300100123002460041180114008a00450022801000911919999805114008964c0080064300100146004280114008a004a660164426601900180008964c018d55ce80148564c018d55cf001c8564d4c0184800400e43001801c00400e0048009180122650023530051200100244c8ca0046a600a24002004600a6aae780082400250010011199999803112a999ab9a3370e900000108c0060041155333573466e1d200200211800c00023002280114008a0045002001223333330062255333573466e1d200000211800c00822a64a666ae68cdc3a400400622b266601500222800801000a42b2600c00321800c0040048980124c89800a4c113001498954ccd5cd19b874801000c4600300408c008a00450022801140080048cccccc0108954ccd5cd19b874800000844c964cc0048a0030010028008005219001912c9a80400148564cc0148a00200400290ac9a805800c8600300100380091801230024600446004300780108c008a00450022801140080044ccccc0088a0045002280114008a0020021333330012280114008a0045001001140088888894cccccd5d2000899198039aab9d00135573c0026ea80044c014dd5800898021bac00113003375a002260046eb80048894ccd55cf80088018998011aba100135744002464600200246004466004004003"


metadataScriptBytesResult : String -> Result String (Bytes.Comparable.Bytes a)
metadataScriptBytesResult mSS =
    case Bytes.Comparable.fromHex mSS of
        Just x ->
            Ok x

        Nothing ->
            Err "error converting stamp script into bytes"


metadataPlutusScriptResult : String -> Result String Cardano.Script.PlutusScript
metadataPlutusScriptResult mMSB =
    Result.map
        (\x ->
            Cardano.Script.plutusScriptFromBytes Cardano.Script.PlutusV3 x
        )
        (metadataScriptBytesResult mMSB)


localStateUtxos : List ( Cardano.Utxo.OutputReference, a ) -> Cardano.Utxo.RefDict a
localStateUtxos myUtxos =
    Cardano.Utxo.refDictFromList myUtxos


metadataScriptResult : String -> Result String Cardano.Script.Script
metadataScriptResult mMSB =
    Result.map Cardano.Script.Plutus
        (metadataPlutusScriptResult
            mMSB
        )


threeAda : Cardano.Value.Value
threeAda =
    Cardano.Value.onlyLovelace (Natural.fromSafeString "3000000")


closeMetadata wallet maybeMetadataOutReferenceAndOutput maybeMetadataReferenceScriptOutReferenceAndOutput maybePubKeyHash =
    case Toop.T4 (metadataScriptResult metadataScriptString) maybeMetadataOutReferenceAndOutput maybeMetadataReferenceScriptOutReferenceAndOutput maybePubKeyHash of
        Toop.T4 (Ok (Cardano.Script.Plutus metadataPlutusScript)) (Just metadataOutReferenceAndOutput) (Just metadataReferenceScriptOutReferenceAndOutput) (Just pubKeyHash) ->
            let
                metadataScriptOutReference =
                    metadataReferenceScriptOutReferenceAndOutput
                        |> Tuple.first

                metadataScriptOutput : Cardano.Utxo.Output
                metadataScriptOutput =
                    metadataReferenceScriptOutReferenceAndOutput
                        |> Tuple.second

                metadata : Cardano.TxOtherInfo
                metadata =
                    Cardano.TxMetadata
                        { tag = Natural.zero
                        , metadata =
                            Cardano.Metadatum.Bytes
                                (Bytes.Comparable.fromText "Close Metadata")
                        }

                plutusScriptWitness : Cardano.PlutusScriptWitness
                plutusScriptWitness =
                    { script =
                        ( Cardano.Script.PlutusV3
                        , Cardano.WitnessByReference metadataScriptOutReference
                        )
                    , redeemerData = \_ -> Cardano.Data.Constr Natural.zero []
                    , requiredSigners = [ pubKeyHash ]
                    }

                referenceEmptyTxBody =
                    Cardano.Transaction.newBody

                referenceTxBody =
                    { referenceEmptyTxBody | outputs = [ metadataScriptOutput ] }

                referenceTransaction =
                    { body = referenceTxBody
                    , witnessSet = Cardano.Transaction.newWitnessSet
                    , isValid = True
                    , auxiliaryData =
                        Just
                            { labels = []
                            , nativeScripts = []
                            , plutusV1Scripts = []
                            , plutusV2Scripts = []
                            , plutusV3Scripts =
                                [ Cardano.Script.cborWrappedBytes metadataPlutusScript
                                ]
                            }
                    }

                metadataTxBody =
                    { referenceEmptyTxBody | outputs = [ metadataOutput ] }

                _ =
                    Debug.log "metadataOutput " metadataOutput

                metadataTransaction =
                    { body = metadataTxBody
                    , witnessSet = Cardano.Transaction.newWitnessSet
                    , isValid = True
                    , auxiliaryData = Nothing
                    }

                ( metadataOutReference, metadataOutput ) =
                    metadataOutReferenceAndOutput

                localUtxosWithReferenceScript =
                    Cardano.updateLocalState metadataScriptOutReference.transactionId
                        referenceTransaction
                        (localStateUtxos wallet.utxos)

                localUtxosWithMetadataScript =
                    Cardano.updateLocalState metadataOutReference.transactionId
                        metadataTransaction
                        localUtxosWithReferenceScript.updatedState
            in
            [ Cardano.Spend
                (Cardano.FromPlutusScript
                    { spentInput = metadataOutReference
                    , datumWitness = Nothing
                    , plutusScriptWitness = plutusScriptWitness
                    }
                )
            , Cardano.SendTo wallet.changeAddress
                threeAda
            ]
                |> (\x ->
                        Cardano.finalize localUtxosWithMetadataScript.updatedState
                            [ metadata
                            ]
                            x
                   )

        Toop.T4 (Err e) _ _ _ ->
            Err (Cardano.FailurePleaseReportToElmCardano "missing address")

        Toop.T4 _ Nothing _ _ ->
            Err (Cardano.FailurePleaseReportToElmCardano "missing metadata utxo")

        Toop.T4 _ _ Nothing _ ->
            Err (Cardano.FailurePleaseReportToElmCardano "missing metadata reference")

        Toop.T4 _ _ _ Nothing ->
            Err (Cardano.FailurePleaseReportToElmCardano "missing pubKeyHash")

        _ ->
            Err (Cardano.FailurePleaseReportToElmCardano "other issue")


main =
    -- The main entry point of our app
    -- More info about that in the Browser package docs:
    -- https://package.elm-lang.org/packages/elm/browser/latest/
    Browser.element
        { init =
            \flagsJson ->
                init flagsJson
                    |> Tuple.mapSecond perform
        , update =
            \msg model ->
                update msg model
                    |> Tuple.mapSecond perform
        , subscriptions = \_ -> fromWallet WalletMsg
        , view = view
        }


port toWallet : Json.Decode.Value -> Cmd msg


port fromWallet : (Json.Decode.Value -> msg) -> Sub msg


type Msg
    = WalletMsg Json.Decode.Value
    | ConnectButtonClicked { id : String }
    | GotProtocolParameters (Result Http.Error ProtocolParams)
    | GotReferenceUtxos (Result Http.Error (List ReferenceScriptUtxo))
    | CloseMetadata
    | ReceiveCloseMetadata (Result Json.Decode.Error (Result Cardano.TxFinalizationError Cardano.TxFinalized))


type Effect
    = DiscoverWallets
    | EnableWallet String
    | GetWalletUtxos Cardano.Cip30.Wallet
    | GetChangeAddress Cardano.Cip30.Wallet
    | GetRewardAddress Cardano.Cip30.Wallet
    | GetProtocalParameters
    | GetReferenceUtxos
    | SubmitCloseMetadataTransaction Wallet Cardano.Address.Address
    | SubmitCloseMetadataScriptTransactionForSignature Cardano.Transaction.Transaction Wallet
    | SubmitCloseMetadataScriptForSubmission Cardano.Transaction.Transaction Wallet
    | NoEffect


maestroAPIKey : Cardano.Address.NetworkId -> String
maestroAPIKey n =
    case n of
        Cardano.Address.Testnet ->
            "V37EokNtPF6ypmACAycFsdX0cGYtiJxI"

        Cardano.Address.Mainnet ->
            "zYa6dQgOTIqHBZiOUvZY2whrEwx1mw6T"


apiKeyHeader : Cardano.Address.NetworkId -> Http.Header
apiKeyHeader n =
    Http.header "api-key" (maestroAPIKey n)


protocolParamsDecoder : Json.Decode.Decoder ProtocolParams
protocolParamsDecoder =
    Json.Decode.map
        (\v3 ->
            { costModels = Cardano.Gov.CostModels Nothing Nothing (Just v3) }
        )
        (Json.Decode.at [ "data", "plutus_cost_models", "plutus_v3" ] <| Json.Decode.list Json.Decode.int)


dataUrl : Cardano.Address.NetworkId -> String
dataUrl n =
    case n of
        Cardano.Address.Testnet ->
            "https://preview.gomaestro-api.org/v1/"

        _ ->
            "https://mainnet.gomaestro-api.org/v1/"


loadProtocolParameters : Cardano.Address.NetworkId -> (Result Http.Error ProtocolParams -> msg) -> Cmd msg
loadProtocolParameters networkId msg =
    Http.request
        { method = "GET"
        , headers =
            [ apiKeyHeader networkId
            ]
        , url = dataUrl networkId ++ "protocol-parameters"
        , body = Http.emptyBody
        , expect =
            Http.expectJson msg protocolParamsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


type alias ReferenceScriptUtxo =
    { txId : String
    , datum : String
    , index : Int
    , referenceScriptHex : String
    }


type alias SpendableScriptUtxo =
    { txId : String
    , index : Int
    , datum : String
    }


getSpendableUtxos : Cardano.Address.NetworkId -> String -> Task.Task Json.Decode.Error (List SpendableScriptUtxo)
getSpendableUtxos networkId address =
    let
        _ =
            Debug.log "getUtxos" ()
    in
    Http.task
        { method = "GET"
        , headers =
            [ apiKeyHeader networkId
            ]
        , url =
            dataUrl networkId
                ++ "addresses/"
                ++ address
                ++ "/utxos"
                ++ "?resolve_datums=true"
        , body = Http.emptyBody
        , resolver =
            utxoResolver
        , timeout = Nothing
        }


decodeReferenceScriptResponse : Json.Decode.Decoder ReferenceScriptUtxo
decodeReferenceScriptResponse =
    Json.Decode.succeed ReferenceScriptUtxo
        |> Json.Decode.Pipeline.required "tx_hash" Json.Decode.string
        |> Json.Decode.Pipeline.requiredAt [ "datum", "json", "bytes" ] Json.Decode.string
        |> Json.Decode.Pipeline.required "index" Json.Decode.int
        |> Json.Decode.Pipeline.requiredAt [ "reference_script", "bytes" ] Json.Decode.string


decodeReferenceScriptResponses : Json.Decode.Decoder (List ReferenceScriptUtxo)
decodeReferenceScriptResponses =
    Json.Decode.field "data"
        (Json.Decode.list decodeReferenceScriptResponse)


decodeSpendableScriptUtxoResponses : Json.Decode.Decoder (List SpendableScriptUtxo)
decodeSpendableScriptUtxoResponses =
    Json.Decode.field "data"
        (Json.Decode.list decodeSpendableScriptUtxoResponse)


decodeSpendableScriptUtxoResponse : Json.Decode.Decoder SpendableScriptUtxo
decodeSpendableScriptUtxoResponse =
    Json.Decode.succeed SpendableScriptUtxo
        |> Json.Decode.Pipeline.required "tx_hash" Json.Decode.string
        |> Json.Decode.Pipeline.required "index" Json.Decode.int
        |> Json.Decode.Pipeline.requiredAt [ "datum", "bytes" ] Json.Decode.string


utxoResolver : Http.Resolver Json.Decode.Error (List SpendableScriptUtxo)
utxoResolver =
    Http.stringResolver
        (\x ->
            let
                _ =
                    Debug.log "here" x
            in
            case x of
                Http.GoodStatus_ _ body ->
                    let
                        _ =
                            Debug.log "body " body
                    in
                    Json.Decode.decodeString
                        decodeSpendableScriptUtxoResponses
                        body

                _ ->
                    Err (Json.Decode.Failure "no bueno" (Json.Encode.string ""))
        )


scriptBytesToResult : String -> Result String (Bytes.Comparable.Bytes a)
scriptBytesToResult mSS =
    case Bytes.Comparable.fromHex mSS of
        Just x ->
            Ok x

        Nothing ->
            Err "error converting mint script into bytes"


plutusScriptToResult : String -> Result String Cardano.Script.PlutusScript
plutusScriptToResult mMSB =
    Result.map
        (\x ->
            Cardano.Script.plutusScriptFromBytes Cardano.Script.PlutusV3 x
        )
        (scriptBytesToResult mMSB)


scriptToResult : String -> Result String Cardano.Script.Script
scriptToResult mMSB =
    Result.map Cardano.Script.Plutus
        (plutusScriptToResult
            mMSB
        )


convertReferenceScriptResponseIntoListOfUTxOReferenceOutputPairs : List ReferenceScriptUtxo -> List ( Cardano.Utxo.OutputReference, Cardano.Utxo.Output )
convertReferenceScriptResponseIntoListOfUTxOReferenceOutputPairs listOfMaestroResponses =
    case maybeReferenceWalletAddress of
        Just referenceWalletAddress ->
            List.map
                (\maestroResponse ->
                    ( { transactionId = Bytes.Comparable.fromHexUnchecked maestroResponse.txId
                      , outputIndex = maestroResponse.index
                      }
                    , { address =
                            referenceWalletAddress
                      , amount = threeAda
                      , datumOption =
                            Maybe.map
                                (\x ->
                                    Cardano.Utxo.datumValueFromData
                                        (Cardano.Data.Bytes
                                            x
                                        )
                                )
                                (Bytes.Comparable.fromHex maestroResponse.datum)
                      , referenceScript =
                            Result.map Cardano.Script.refFromScript
                                (scriptToResult maestroResponse.referenceScriptHex)
                                |> Result.toMaybe
                      }
                    )
                )
                listOfMaestroResponses

        Nothing ->
            []


convertSpendableUtxoIntoListOfUTxOReferenceOutputPairs : Cardano.Address.Address -> List SpendableScriptUtxo -> List ( Cardano.Utxo.OutputReference, Cardano.Utxo.Output )
convertSpendableUtxoIntoListOfUTxOReferenceOutputPairs address listOfMaestroResponses =
    let
        _ =
            Debug.log "listOfMaestroResponses " listOfMaestroResponses
    in
    List.map
        (\maestroResponse ->
            ( { transactionId = Bytes.Comparable.fromHexUnchecked maestroResponse.txId
              , outputIndex = maestroResponse.index
              }
            , { address =
                    address
              , amount = threeAda
              , datumOption =
                    Bytes.Comparable.fromHex maestroResponse.datum
                        |> Maybe.andThen Cardano.Data.fromBytes
                        |> Maybe.map
                            (\x ->
                                Cardano.Utxo.datumValueFromData
                                    x
                            )
              , referenceScript =
                    Nothing
              }
            )
        )
        listOfMaestroResponses


getReferenceScriptUtxos : Cardano.Address.NetworkId -> (Result Http.Error (List ReferenceScriptUtxo) -> msg) -> Cmd msg
getReferenceScriptUtxos networkId receivedRefereceWalletUtxosMsg =
    Http.request
        { method = "GET"
        , headers =
            [ apiKeyHeader networkId
            ]
        , url =
            (dataUrl networkId ++ "addresses/")
                ++ referenceWalletAddressText
                ++ "/utxos"
                ++ "?asset="
                ++ referencePolicyIdHex
                ++ referenceTokenAssetNameHex
                ++ "&resolve_datums=true"
        , body = Http.emptyBody
        , expect =
            Http.expectJson receivedRefereceWalletUtxosMsg
                decodeReferenceScriptResponses
        , timeout = Nothing
        , tracker = Nothing
        }


perform effect =
    case effect of
        DiscoverWallets ->
            toWallet <| Cardano.Cip30.encodeRequest Cardano.Cip30.discoverWallets

        EnableWallet id ->
            toWallet (Cardano.Cip30.encodeRequest (Cardano.Cip30.enableWallet { id = id, extensions = [] }))

        GetWalletUtxos walletCip ->
            toWallet
                (Cardano.Cip30.encodeRequest
                    (Cardano.Cip30.getUtxos walletCip { amount = Nothing, paginate = Nothing })
                )

        GetRewardAddress walletCip ->
            toWallet (Cardano.Cip30.encodeRequest (Cardano.Cip30.getRewardAddresses walletCip))

        GetChangeAddress walletCip ->
            toWallet (Cardano.Cip30.encodeRequest (Cardano.Cip30.getChangeAddress walletCip))

        GetProtocalParameters ->
            loadProtocolParameters
                Cardano.Address.Testnet
                GotProtocolParameters

        GetReferenceUtxos ->
            getReferenceScriptUtxos network GotReferenceUtxos

        SubmitCloseMetadataTransaction wallet metadataScriptAddress ->
            getSpendableUtxos network (Cardano.Address.toBech32 metadataScriptAddress)
                |> Task.andThen
                    (\metadataUtxos ->
                        let
                            outputMetadataUtxos : List ( Cardano.Utxo.OutputReference, Cardano.Utxo.Output )
                            outputMetadataUtxos =
                                convertSpendableUtxoIntoListOfUTxOReferenceOutputPairs
                                    metadataScriptAddress
                                    metadataUtxos

                            _ =
                                Debug.log "outputMetadataUtxos " outputMetadataUtxos

                            maybeMetadataScriptReference =
                                List.filter
                                    (\( outRef, output ) ->
                                        case output.datumOption of
                                            Just (Cardano.Utxo.DatumValue { rawBytes }) ->
                                                let
                                                    datum =
                                                        Bytes.Comparable.toBytes rawBytes
                                                            |> Cbor.Decode.decode Cardano.Data.fromCbor
                                                            |> Maybe.withDefault (Cardano.Data.List [])
                                                in
                                                datum
                                                    == Cardano.Data.Bytes
                                                        (Bytes.Comparable.fromText "Metadata")

                                            _ ->
                                                False
                                    )
                                    wallet.referenceUtxos
                                    |> List.head

                            maybePubKeyHash =
                                Cardano.Address.extractPubKeyHash wallet.changeAddress
                        in
                        Task.succeed
                            (closeMetadata
                                wallet
                                (List.head (List.reverse outputMetadataUtxos))
                                maybeMetadataScriptReference
                                maybePubKeyHash
                            )
                    )
                |> Task.attempt ReceiveCloseMetadata

        SubmitCloseMetadataScriptTransactionForSignature goodTx wallet ->
            toWallet
                (Cardano.Cip30.encodeRequest
                    (Cardano.Cip30.signTx wallet.wallet
                        { partialSign = True }
                        goodTx
                    )
                )

        SubmitCloseMetadataScriptForSubmission signedTx wallet ->
            toWallet (Cardano.Cip30.encodeRequest (Cardano.Cip30.submitTx wallet.wallet signedTx))

        NoEffect ->
            Cmd.none



-- MODEL


type alias ProtocolParams =
    { costModels : Cardano.Gov.CostModels
    }


type alias Wallet =
    { wallet : Cardano.Cip30.Wallet
    , utxos : List ( Cardano.Utxo.OutputReference, Cardano.Utxo.Output )
    , changeAddress : Cardano.Address.Address
    , rewardAddresses : List Cardano.Address.Address
    , protocolParameters : ProtocolParams
    , referenceUtxos : List ( Cardano.Utxo.OutputReference, Cardano.Utxo.Output )
    }


type Model
    = Startup
    | WalletDiscovered (List Cardano.Cip30.WalletDescriptor)
    | WalletLoading
        { wallet : Cardano.Cip30.Wallet
        , utxos : List ( Cardano.Utxo.OutputReference, Cardano.Utxo.Output )
        }
    | WalletLoaded
        { wallet : Cardano.Cip30.Wallet
        , utxos : List ( Cardano.Utxo.OutputReference, Cardano.Utxo.Output )
        , changeAddress : Cardano.Address.Address
        }
    | WalletWithChangeAddress
        { wallet : Cardano.Cip30.Wallet
        , utxos : List ( Cardano.Utxo.OutputReference, Cardano.Utxo.Output )
        , changeAddress : Cardano.Address.Address
        }
    | WalletWithRewardAddress
        { wallet : Cardano.Cip30.Wallet
        , utxos : List ( Cardano.Utxo.OutputReference, Cardano.Utxo.Output )
        , changeAddress : Cardano.Address.Address
        , rewardAddresses : List Cardano.Address.Address
        }
    | GettingProtocalParameters
        { wallet : Cardano.Cip30.Wallet
        , utxos : List ( Cardano.Utxo.OutputReference, Cardano.Utxo.Output )
        , changeAddress : Cardano.Address.Address
        , rewardAddresses : List Cardano.Address.Address
        }
    | GettingReferenceUtxos
        { wallet : Cardano.Cip30.Wallet
        , utxos : List ( Cardano.Utxo.OutputReference, Cardano.Utxo.Output )
        , changeAddress : Cardano.Address.Address
        , protocolParameters : ProtocolParams
        , rewardAddresses : List Cardano.Address.Address
        }
    | ClosingMetadataScript Cardano.Transaction.Transaction Wallet
    | ClosedMetadataScript (Bytes.Comparable.Bytes Cardano.Utxo.TransactionId) Wallet
    | WaitingForInput Wallet


init : () -> ( Model, Effect )
init _ =
    ( Startup
    , DiscoverWallets
    )


referencePolicyIdHex : String
referencePolicyIdHex =
    case network of
        Cardano.Address.Mainnet ->
            ""

        Cardano.Address.Testnet ->
            "d4c84babcb60f1e301fa4fd1c1759a2ebcd1d346988418ac459088e4"


maybeReferenceTokenPolicyId : Maybe (Bytes.Comparable.Bytes a)
maybeReferenceTokenPolicyId =
    Bytes.Comparable.fromHex referencePolicyIdHex


maybeStarcadaReferenceToken : Maybe (Cardano.MultiAsset.MultiAsset Natural.Natural)
maybeStarcadaReferenceToken =
    Maybe.map
        (\referenceTokenPolicyId ->
            Cardano.MultiAsset.onlyToken
                referenceTokenPolicyId
                referenceTokenAssetName
                Natural.one
        )
        maybeReferenceTokenPolicyId


referenceTokenAssetNameText =
    "StarcadaReferenceToken"


referenceTokenAssetNameHex =
    referenceTokenAssetNameText
        |> Bytes.Comparable.fromText
        |> Bytes.Comparable.toHex


referenceTokenAssetName =
    Bytes.Comparable.fromText referenceTokenAssetNameText


maybeDeployScriptValue =
    Maybe.map
        (\starcadaReferenceToken ->
            { lovelace = Natural.fromSafeString "50000000"
            , assets =
                starcadaReferenceToken
            }
        )
        maybeStarcadaReferenceToken



-- UPDATE


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case ( msg, model ) of
        ( WalletMsg value, _ ) ->
            case ( Json.Decode.decodeValue Cardano.Cip30.responseDecoder value, model ) of
                -- We just discovered available wallets
                ( Ok (Cardano.Cip30.AvailableWallets wallets), Startup ) ->
                    ( WalletDiscovered wallets, NoEffect )

                -- We just connected to the wallet, let’s ask for the available utxos
                ( Ok (Cardano.Cip30.EnabledWallet wallet), WalletDiscovered _ ) ->
                    ( WalletLoading { wallet = wallet, utxos = [] }
                    , GetWalletUtxos wallet
                    )

                -- We just received the utxos, let’s ask for the main change address of the wallet
                ( Ok (Cardano.Cip30.ApiResponse { walletId } (Cardano.Cip30.WalletUtxos utxos)), WalletLoading { wallet } ) ->
                    ( WalletLoading { wallet = wallet, utxos = utxos }
                    , GetChangeAddress wallet
                    )

                ( Ok (Cardano.Cip30.ApiResponse { walletId } (Cardano.Cip30.ChangeAddress address)), WalletLoading { wallet, utxos } ) ->
                    ( WalletWithChangeAddress { wallet = wallet, utxos = utxos, changeAddress = address }
                    , GetRewardAddress wallet
                    )

                ( Ok (Cardano.Cip30.ApiResponse { walletId } (Cardano.Cip30.RewardAddresses rewardAddresses)), WalletWithChangeAddress previousWallet ) ->
                    ( GettingProtocalParameters
                        { wallet = previousWallet.wallet
                        , utxos = previousWallet.utxos
                        , changeAddress = previousWallet.changeAddress
                        , rewardAddresses = rewardAddresses
                        }
                    , GetProtocalParameters
                    )

                _ ->
                    ( model, NoEffect )

        ( GotReferenceUtxos result, GettingReferenceUtxos previousWallet ) ->
            case result of
                Ok maestroResponses ->
                    ( WaitingForInput
                        { wallet = previousWallet.wallet
                        , utxos = previousWallet.utxos
                        , changeAddress = previousWallet.changeAddress
                        , rewardAddresses = previousWallet.rewardAddresses
                        , protocolParameters = previousWallet.protocolParameters
                        , referenceUtxos =
                            convertReferenceScriptResponseIntoListOfUTxOReferenceOutputPairs
                                maestroResponses
                        }
                    , NoEffect
                    )

                Err err ->
                    ( model, NoEffect )

        ( ConnectButtonClicked { id }, WalletDiscovered descriptors ) ->
            ( model, EnableWallet id )

        ( CloseMetadata, WaitingForInput wallet ) ->
            case metadataScriptResult metadataScriptString of
                Ok metadataScript ->
                    let
                        metadataScriptHash =
                            Cardano.Script.hash metadataScript

                        metadataScriptAddress : Cardano.Address.Address
                        metadataScriptAddress =
                            Cardano.Address.script
                                Cardano.Address.Testnet
                                metadataScriptHash
                    in
                    ( model, SubmitCloseMetadataTransaction wallet metadataScriptAddress )

                Err err ->
                    ( model, NoEffect )

        ( GotProtocolParameters result, GettingProtocalParameters previousWallet ) ->
            case result of
                Ok protocolParameters ->
                    ( GettingReferenceUtxos
                        { wallet = previousWallet.wallet
                        , utxos = previousWallet.utxos
                        , changeAddress = previousWallet.changeAddress
                        , rewardAddresses = previousWallet.rewardAddresses
                        , protocolParameters = protocolParameters
                        }
                    , GetReferenceUtxos
                    )

                Err err ->
                    ( model, NoEffect )

        ( ReceiveCloseMetadata x, WaitingForInput wallet ) ->
            let
                _ =
                    Debug.log "in RecieveCloseMetadata " x
            in
            case x of
                Ok (Ok validTx) ->
                    ( ClosingMetadataScript validTx.tx wallet
                    , SubmitCloseMetadataScriptTransactionForSignature validTx.tx wallet
                    )

                _ ->
                    ( model, NoEffect )

        _ ->
            ( model, NoEffect )



-- VIEW


view : Model -> Html.Html Msg
view model =
    case model of
        Startup ->
            Html.div [] [ Html.div [] [ Html.text "Hello Cardano!" ] ]

        WalletDiscovered availableWallets ->
            Html.div []
                [ Html.div [] [ Html.text "Hello Cardano!" ]
                , Html.div [] [ Html.text "CIP-30 wallets detected:" ]
                , viewAvailableWallets availableWallets
                ]

        WalletLoading _ ->
            Html.div [] [ Html.text "Loading wallet assets ..." ]

        WaitingForInput { wallet, utxos, changeAddress } ->
            Html.div []
                [ Html.div [] [ Html.text <| "Wallet: " ++ (Cardano.Cip30.walletDescriptor wallet).name ]
                , Html.div [] [ Html.text <| "Address: " ++ (Cardano.Address.toBytes changeAddress |> Bytes.Comparable.toHex) ]
                , Html.div [] [ Html.text <| "UTxO count: " ++ String.fromInt (List.length utxos) ]
                , Html.button [ Html.Events.onClick CloseMetadata ] [ Html.text "close utxo at script" ]
                ]

        _ ->
            Html.div [] []


viewAvailableWallets : List Cardano.Cip30.WalletDescriptor -> Html.Html Msg
viewAvailableWallets wallets =
    let
        walletDescription : Cardano.Cip30.WalletDescriptor -> String
        walletDescription w =
            "id: " ++ w.id ++ ", name: " ++ w.name

        walletIcon : Cardano.Cip30.WalletDescriptor -> Html.Html Msg
        walletIcon { icon } =
            Html.img [ Html.Attributes.src icon, Html.Attributes.height 32 ] []

        connectButton { id } =
            Html.button [ Html.Events.onClick (ConnectButtonClicked { id = id }) ] [ Html.text "connect" ]

        walletRow w =
            Html.div [] [ walletIcon w, Html.text (walletDescription w), connectButton w ]
    in
    Html.div [] (List.map walletRow wallets)
