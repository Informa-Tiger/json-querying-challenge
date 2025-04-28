<script lang="ts">
  import { Styles } from "@sveltestrap/sveltestrap";
	let { children } = $props();
</script>

<Styles theme="auto"/>

{@render children()}