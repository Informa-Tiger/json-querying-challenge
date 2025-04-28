<script lang="ts">
    import Time from "svelte-time";
    import {
        Button,
        Input,
        InputGroup,
        Table,
        Badge,
        Spinner,
        Card,
    } from "@sveltestrap/sveltestrap";

    let querystr = $state("");
    let query: Promise<any[][]> | undefined = $state();
    let history: any[] = $state([]);
    let selectedId: number | null = $state(null);
    let selected = $derived(history.find((q) => q.id === selectedId));
    import { PUBLIC_API as api } from "$env/static/public";

    function runQuery() {
        query = fetch(api + "/query", { method: "POST", body: querystr }).then(
            (response) => {
                fetchHistory();
                return new Promise(function (resolve, reject) {
                    if (!response.ok)
                        response.text().then(reject).catch(reject);
                    else return response.json().then(resolve).catch(reject);
                });
            },
        );
    }

    function fetchHistory() {
        fetch(api + "/history")
            .then((response) => response.json())
            .then((result) => (history = result));
    }

    fetchHistory();
</script>

{#snippet resultTable(result: any[][])}
    <Table>
        <tbody>
            {#if result.length > 0}
                {#each result as row}
                    <tr>
                        {#each row as column}
                            <td>{column}</td>
                        {/each}
                    </tr>
                {/each}
            {:else}
                <Card body>empty result set</Card>
            {/if}
        </tbody>
    </Table>
{/snippet}

<div class="container">
    <div class="row" onclick={() => (selectedId = null)}>
        <div class="col">
            <InputGroup>
                <Input
                    bind:value={querystr}
                    type="text"
                    placeholder="SQL query"
                    onkeydown={(e) => {
                        if (e.key == "Enter") runQuery();
                    }}
                />
                <Button onclick={runQuery}>Send</Button>
            </InputGroup>
        </div>
    </div>

    <div class="row" style="overflow: hidden; flex: 1;">
        <div class="col">
            <div class="table-responsive">
                <Table>
                    <thead style="position: sticky; top: 0;">
                        <tr>
                            <th>Timestamp</th>
                            <th>Query</th>
                            <th>Status</th>
                        </tr>
                    </thead>
                    <tbody>
                        {#each history as query (query.id)}
                            <tr
                                onclick={() => (selectedId = query.id)}
                                class:table-primary={selectedId == query.id}
                            >
                                <td>
                                    <Time
                                        timestamp={query.timestamp}
                                        format="YY-MM-DD HH:mm"
                                    />
                                </td>
                                <td>{query.query}</td>
                                <td>
                                    <Badge
                                        color={query.success
                                            ? "success"
                                            : "danger"}
                                        >{query.success ? "Ok" : "Error"}
                                    </Badge>
                                </td>
                            </tr>
                        {/each}
                    </tbody>
                </Table>
            </div>
        </div>

        <div class="col">
            <h3>Result</h3>
            {#if selectedId !== null}
                {#if selected.success}
                    {@render resultTable(JSON.parse(selected.result))}
                {:else}
                    <Card body color="danger">{selected.result}</Card>
                {/if}
            {:else if query === undefined}
                <Card body>Send a query</Card>
            {:else}
                {#await query}
                    <Spinner />
                {:then result}
                    {@render resultTable(result)}
                {:catch err}
                    <Card body color="danger">{err}</Card>
                {/await}
            {/if}
        </div>
    </div>
</div>

<style>
    .row {
        margin: 1em 0 1em 0;
    }

    .col {
        height: 100%;
    }

    .table-responsive {
        height: 100%;
    }

    .container {
        height: 100%;
        overflow: hidden;
        display: flex;
        flex-direction: column;
    }
</style>
